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

import PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint, DatabaseUnavailable)
  , PaymentError(AlreadyPaid)
  , VoucherDatabase(payForVoucher, redeemVoucher, redeemVoucherWithCounter)
  , VoucherDatabaseState(SQLiteDB)
  , memory
  , sqlite
  , upgradeSchema
  , latestVersion
  , readVersion
  )

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
voucher = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
anotherVoucher = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
fingerprint = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
anotherFingerprint = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

-- Mock a successful payment.
paySuccessfully :: IO ()
paySuccessfully = return ()

-- Mock a failed payment.
failPayment :: IO ()
failPayment = throwIO ArbitraryException

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
      () <- payForVoucher conn voucher paySuccessfully
      result <- redeemVoucher conn voucher fingerprint
      assertEqual "redeeming paid voucher" (Right True) result
  , testCase "allowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher fingerprint
      first <- redeem
      second <- redeem
      assertEqual "redeeming paid voucher" (Right True) first
      assertEqual "re-redeeming paid voucher" (Right False) second
  , testCase "disallowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher
      first <- redeem fingerprint
      second <- redeem (Text.cons 'a' $ Text.tail fingerprint)
      assertEqual "redeeming paid voucher" (Right True) first
      assertEqual "re-redeeming paid voucher" (Left AlreadyRedeemed) second
  , testCase "allowed redemption varying by counter" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem anotherFingerprint 1
      assertEqual "redeemed with counter 0" (Right True) first
      assertEqual "redeemed with counter 1" (Right True) second
  , testCase "disallowed redemption varying by counter but not fingerprint" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem fingerprint 1
      assertEqual "redeemed with counter 0" (Right True) first
      assertEqual "redeemed with counter 1" (Left DuplicateFingerprint) second
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
      () <- pay
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

      assertEqual "Both payments should succeed" ((), ()) result
  , testCase "concurrent redemption" $ do
      connect <- makeDatabase
      connA <- connect
      connB <- connect
      -- It doesn't matter which connection pays for the vouchers.  They
      -- payments are concurrent and the connections are to the same database.
      () <- payForVoucher connA voucher paySuccessfully
      () <- payForVoucher connA anotherVoucher paySuccessfully

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
      (path, handle) <- openTempFile tempdir "voucher-.db"
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
      upgradeSchema latestVersion conn
      let expected = Right latestVersion
      actual <- readVersion conn
      assertEqual "The recorded schema version should be the latest value" expected actual
  ]
