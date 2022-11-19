{-# LANGUAGE OverloadedStrings #-}

-- | Tests related to PaymentServer.Persistence and the persistence system in
-- general.

module Persistence
  ( tests
  ) where

import qualified Data.Text as Text

import Control.Exception
  ( Exception
  , throwIO
  , try
  )

import Control.Concurrent.Async
  ( withAsync
  , waitBoth
  )

import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  , assertEqual
  )

import System.IO
  ( openTempFile
  )
import System.Directory
  ( getTemporaryDirectory
  )

import qualified Database.SQLite.Simple as Sqlite

import Web.Stripe.Types
  ( ChargeId(ChargeId)
  )
import Web.Stripe.Error
  ( StripeErrorType(CardError)
  , StripeError(StripeError)
  )

import PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint, DatabaseUnavailable)
  , PaymentError(AlreadyPaid, PaymentFailed)
  , VoucherDatabase(payForVoucher, redeemVoucher, redeemVoucherWithCounter)
  , VoucherDatabaseState(SQLiteDB)
  , ProcessorResult
  , memory
  , sqlite
  , upgradeSchema
  , latestVersion
  , readVersion
  )

import Control.Monad (void)

data ArbitraryException = ArbitraryException
  deriving (Show, Eq)

instance Exception ArbitraryException

tests :: TestTree
tests = testGroup "Persistence"
  [ memoryDatabaseVoucherPaymentTests
  , sqlite3DatabaseVoucherPaymentTests
  , sqlite3DatabaseSchemaTests
  ]

-- Some dummy values that should be replaced by the use of QuickCheck.
voucher :: Voucher
voucher = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
anotherVoucher :: Voucher
anotherVoucher = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
fingerprint :: Fingerprint
fingerprint = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
anotherFingerprint :: Fingerprint
anotherFingerprint = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

aChargeId :: ChargeId
aChargeId = ChargeId "abc"

-- Mock a successful payment.
paySuccessfully :: IO (ProcessorResult ChargeId)
paySuccessfully = return . Right $ aChargeId

-- Mock a failed payment.
failPayment :: IO (ProcessorResult ChargeId)
failPayment = throwIO ArbitraryException

-- Mock a payment that fails at the processor rather than with an IO
-- exception.
aStripeError :: StripeError
aStripeError = StripeError CardError "Card rejected because reasons" Nothing Nothing Nothing
failPaymentProcessing :: IO (ProcessorResult ChargeId)
failPaymentProcessing = return $ Left $ PaymentFailed aStripeError

-- | Create a group of tests related to voucher payment and redemption.
makeVoucherPaymentTests
  :: VoucherDatabase d
  => Text.Text           -- ^ A distinctive identifier for this group's label.
  -> IO (IO d)           -- ^ An operation that creates a new, empty voucher
                         -- database and results in an operation that creates
                         -- a new connection to that database.
  -> TestTree
makeVoucherPaymentTests label makeDatabase =
  testGroup ("voucher payments (" ++ Text.unpack label ++ ")")
  [ testCase "not paid for" $ do
      connect <- makeDatabase
      conn <- connect
      result <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming unpaid voucher" (Left NotPaid) result
  , testCase "paid for" $ do
      connect <- makeDatabase
      conn <- connect
      Right _ <- payForVoucher conn voucher paySuccessfully
      result <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming paid voucher" (Right True) result
  , testCase "allowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      Right _ <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher fingerprint
      first <- redeem
      second <- redeem
      assertEqual "redeeming paid voucher" (Right True) first
      assertEqual "re-redeeming paid voucher" (Right False) second
  , testCase "disallowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      Right _ <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher
      first <- redeem fingerprint
      second <- redeem (Text.cons 'a' $ Text.tail fingerprint)
      assertEqual "redeeming paid voucher" (Right True) first
      assertEqual "re-redeeming paid voucher" (Left AlreadyRedeemed) second
  , testCase "allowed redemption varying by counter" $ do
      connect <- makeDatabase
      conn <- connect
      Right _ <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem anotherFingerprint 1
      assertEqual "redeemed with counter 0" (Right True) first
      assertEqual "redeemed with counter 1" (Right True) second
  , testCase "disallowed redemption varying by counter but not fingerprint" $ do
      connect <- makeDatabase
      conn <- connect
      Right _ <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem fingerprint 1
      assertEqual "redeemed with counter 0" (Right True) first
      assertEqual "redeemed with counter 1" (Left DuplicateFingerprint) second
  , testCase "pay with processor error" $ do
      connect <- makeDatabase
      conn <- connect
      actual <- payForVoucher conn voucher failPaymentProcessing
      let expected = Left $ PaymentFailed aStripeError
      assertEqual "failing payment processing for a voucher" expected actual
      result <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming voucher with failed payment" (Left NotPaid) result
  , testCase "pay with exception" $ do
      connect <- makeDatabase
      conn <- connect
      payResult <- try $ payForVoucher conn voucher failPayment
      assertEqual "failing a payment for a voucher" (Left ArbitraryException) payResult
      result <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming voucher with failed payment" (Left NotPaid) result
  , testCase "disallowed double payment" $ do
      connect <- makeDatabase
      conn <- connect
      let pay = payForVoucher conn voucher paySuccessfully
      Right _ <- pay
      payResult <- try pay
      assertEqual "double-paying for a voucher" (Left AlreadyPaid) payResult
      redeemResult <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming double-paid voucher" (Right True) redeemResult
  , testCase "concurrent payment" $ do
      connect <- makeDatabase
      connA <- connect
      connB <- connect

      let payment = payForVoucher connA voucher paySuccessfully
      let anotherPayment = payForVoucher connB anotherVoucher paySuccessfully

      result <- withAsync payment $ \p1 -> do
        withAsync anotherPayment $ \p2 -> do
          waitBoth p1 p2

      assertEqual "Both payments should succeed" (Right aChargeId, Right aChargeId) result
  , testCase "concurrent redemption" $ do
      connect <- makeDatabase
      connA <- connect
      connB <- connect
      -- It doesn't matter which connection pays for the vouchers.  They
      -- payments are concurrent and the connections are to the same database.
      Right _ <- payForVoucher connA voucher paySuccessfully
      Right _ <- payForVoucher connA anotherVoucher paySuccessfully

      -- It does matter which connection is used to redeem the voucher.  A
      -- connection can only do one thing at a time.
      let redeem = redeemVoucher connA voucher fingerprint
      let anotherRedeem = redeemVoucher connB anotherVoucher anotherFingerprint

      result <- withAsync redeem $ \r1 -> do
        withAsync anotherRedeem $ \r2 -> do
          waitBoth r1 r2

      assertEqual "Both redemptions should succeed" (Right True, Right True) result
  ]

-- | Instantiate the persistence tests for the memory backend.
memoryDatabaseVoucherPaymentTests :: TestTree
memoryDatabaseVoucherPaymentTests = makeVoucherPaymentTests "memory" $ do
  db <- memory
  return $ return db

-- | Instantiate the persistence tests for the sqlite3 backend.
sqlite3DatabaseVoucherPaymentTests :: TestTree
sqlite3DatabaseVoucherPaymentTests =
  testGroup ""
  [ genericTests
  , sqlite3Tests
  ]
  where
    makeDatabase = do
      tempdir <- getTemporaryDirectory
      (path, _handle) <- openTempFile tempdir "voucher-.db"
      return . sqlite . Text.pack $ path

    genericTests = makeVoucherPaymentTests "sqlite3" makeDatabase

    sqlite3Tests =
      testGroup "SQLite3-specific voucher"
      [ testCase "database is busy" $ do
          aDatabase <- makeDatabase
          normalConnection <- aDatabase
          case normalConnection of
            (SQLiteDB connect) -> do
              -- Acquire a write lock before letting the application code run
              -- so that the application code is denied the write lock.
              normalConn <- connect
              fastBusyConn <- fastBusyConnection connect
              Sqlite.withExclusiveTransaction normalConn $ do
                let expected = Left DatabaseUnavailable
                result <- redeemVoucher fastBusyConn voucher fingerprint
                assertEqual "Redeeming voucher while database busy" expected result
            _ -> error "srsly, what?" -- XXX does this need explicit connection closing?
      ]
      where
        fastBusyConnection
          :: IO Sqlite.Connection
          -> IO VoucherDatabaseState
        fastBusyConnection connect = do
          conn <- connect
          -- Tweak the timeout down so the test completes quickly
          Sqlite.execute_ conn "PRAGMA busy_timeout = 0"
          return . SQLiteDB . return $ conn


sqlite3DatabaseSchemaTests :: TestTree
sqlite3DatabaseSchemaTests =
  testGroup "SQLite3 schema"
  [ testCase "initialize empty database" $
    -- upgradeSchema can start from nothing and upgrade the database to any
    -- defined schema version.  We upgrade to the latest version because that
    -- implies upgrading all the intermediate versions.  It probably wouldn't
    -- hurt to target every intermediate version specifically, though.  I
    -- think that's what SmallCheck is for?
    Sqlite.withConnection ":memory:" $ \conn -> do
      void $ upgradeSchema latestVersion conn
      let expected = Right latestVersion
      actual <- readVersion conn
      assertEqual "The recorded schema version should be the latest value" expected actual

  , testCase "identify version 0" $
    -- readVersion identifies an empty database schema as version 0
    Sqlite.withConnection ":memory:" $ \conn -> do
      let expected = Right 0
      actual <- readVersion conn
      assertEqual "An empty database schema is version 0" expected actual

  , testCase "identify version 1" $
    -- readVersion identifies schema version 1
    Sqlite.withConnection ":memory:" $ \conn -> do
      void $ upgradeSchema 1 conn
      let expected = Right 1
      actual <- readVersion conn
      assertEqual "readVersion identifies database schema version 1" expected actual

  , testCase "identify version 2" $
    -- readVersion identifies schema version 1
    Sqlite.withConnection ":memory:" $ \conn -> do
      void $ upgradeSchema 2 conn
      let expected = Right 2
      actual <- readVersion conn
      assertEqual "readVersion identifies database schema version 2" expected actual

  ]
