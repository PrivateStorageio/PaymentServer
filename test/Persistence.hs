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

import PaymentServer.Persistence
  ( Voucher
  , Fingerprint
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint)
  , PaymentError(AlreadyPaid)
  , VoucherDatabase(payForVoucher, redeemVoucher, redeemVoucherWithCounter)
  , memory
  , sqlite
  )

data ArbitraryException = ArbitraryException
  deriving (Show, Eq)

instance Exception ArbitraryException

tests :: TestTree
tests = testGroup "Persistence"
  [ memoryDatabaseVoucherPaymentTests
  , sqlite3DatabaseVoucherPaymentTests
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
      assertEqual "redeeming paid voucher" (Right ()) result
  , testCase "allowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher fingerprint
      first <- redeem
      second <- redeem
      assertEqual "redeeming paid voucher" (Right ()) first
      assertEqual "re-redeeming paid voucher" (Right ()) second
  , testCase "disallowed double redemption" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucher conn voucher
      first <- redeem fingerprint
      second <- redeem (Text.cons 'a' $ Text.tail fingerprint)
      assertEqual "redeeming paid voucher" (Right ()) first
      assertEqual "re-redeeming paid voucher" (Left AlreadyRedeemed) second
  , testCase "allowed redemption varying by counter" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem anotherFingerprint 1
      assertEqual "redeemed with counter 0" (Right ()) first
      assertEqual "redeemed with counter 1" (Right ()) second
  , testCase "disallowed redemption varying by counter but not fingerprint" $ do
      connect <- makeDatabase
      conn <- connect
      () <- payForVoucher conn voucher paySuccessfully
      let redeem = redeemVoucherWithCounter conn voucher
      first <- redeem fingerprint 0
      second <- redeem fingerprint 1
      assertEqual "redeemed with counter 0" (Right ()) first
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
      assertEqual "redeeming double-paid voucher" (Right ()) redeemResult
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

      assertEqual "Both redemptions should succeed" (Right (), Right ()) result
  ]

-- | Instantiate the persistence tests for the memory backend.
memoryDatabaseVoucherPaymentTests :: TestTree
memoryDatabaseVoucherPaymentTests = makeVoucherPaymentTests "memory" $ do
  db <- memory
  return $ return db

-- | Instantiate the persistence tests for the sqlite3 backend.
sqlite3DatabaseVoucherPaymentTests :: TestTree
sqlite3DatabaseVoucherPaymentTests =
  makeVoucherPaymentTests "sqlite3" $
  do
    tempdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempdir "voucher-.db"
    return . sqlite . Text.pack $ path
