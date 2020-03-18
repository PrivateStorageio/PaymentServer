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
  , RedeemError(NotPaid, AlreadyRedeemed)
  , PaymentError(AlreadyPaid)
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , memory
  , getDBConnection
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
  -> IO d                -- ^ An operation that creates a new, empty voucher
                         -- database.
  -> TestTree
makeVoucherPaymentTests label makeDatabase =
  testGroup ("voucher payments (" ++ Text.unpack label ++ ")")
  [ testCase "not paid for" $ do
      db <- makeDatabase
      result <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming unpaid voucher" (Left NotPaid) result
  , testCase "paid for" $ do
      db <- makeDatabase
      () <- payForVoucher db voucher paySuccessfully
      result <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming paid voucher" (Right ()) result
  , testCase "allowed double redemption" $ do
      db <- makeDatabase
      () <- payForVoucher db voucher paySuccessfully
      let redeem = redeemVoucher db voucher fingerprint
      first <- redeem
      second <- redeem
      assertEqual "redeeming paid voucher" (Right ()) first
      assertEqual "re-redeeming paid voucher" (Right ()) second
  , testCase "disallowed double redemption" $ do
      db <- makeDatabase
      () <- payForVoucher db voucher paySuccessfully
      let redeem = redeemVoucher db voucher
      first <- redeem fingerprint
      second <- redeem (Text.cons 'a' $ Text.tail fingerprint)
      assertEqual "redeeming paid voucher" (Right ()) first
      assertEqual "re-redeeming paid voucher" (Left AlreadyRedeemed) second
  , testCase "pay with exception" $ do
      db <- makeDatabase
      payResult <- try $ payForVoucher db voucher failPayment
      assertEqual "failing a payment for a voucher" (Left ArbitraryException) payResult
      result <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming voucher with failed payment" (Left NotPaid) result
  , testCase "disallowed double payment" $ do
      db <- makeDatabase
      let pay = payForVoucher db voucher paySuccessfully
      () <- pay
      payResult <- try pay
      assertEqual "double-paying for a voucher" (Left AlreadyPaid) payResult
      redeemResult <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming double-paid voucher" (Right ()) redeemResult
  , testCase "concurrent redemption" $ do
      db <- makeDatabase
      () <- payForVoucher db voucher paySuccessfully
      () <- payForVoucher db anotherVoucher paySuccessfully

      let redeem = redeemVoucher db voucher fingerprint
      let anotherRedeem = redeemVoucher db anotherVoucher fingerprint

      result <- withAsync redeem $ \r1 -> do
        withAsync anotherRedeem $ \r2 -> do
          waitBoth r1 r2

      assertEqual "Both redemptions should succeed" (Right (), Right ()) result
  ]

-- | Instantiate the persistence tests for the memory backend.
memoryDatabaseVoucherPaymentTests :: TestTree
memoryDatabaseVoucherPaymentTests = makeVoucherPaymentTests "memory" memory

-- | Instantiate the persistence tests for the sqlite3 backend.
sqlite3DatabaseVoucherPaymentTests :: TestTree
sqlite3DatabaseVoucherPaymentTests =
  makeVoucherPaymentTests "sqlite3" $
  do
    tempdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempdir "voucher-.db"
    getDBConnection $ Text.pack path
