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
  , catch
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

voucher = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
fingerprint = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

paySuccessfully = return ()
failPayment = throwIO ArbitraryException

makeVoucherPaymentTests
  :: VoucherDatabase d
  => Text.Text
  -> IO d
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
  , testCase "pay with error" $ do
      db <- makeDatabase
      payForVoucher db voucher failPayment
        `catch` assertEqual "failing a payment for a voucher" ArbitraryException
      result <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming voucher with failed payment" (Left NotPaid) result
  , testCase "disallowed double payment" $ do
      db <- makeDatabase
      let pay = payForVoucher db voucher paySuccessfully
      () <- pay
      pay `catch`  assertEqual "double-paying for a voucher" AlreadyPaid
      redeemResult <- redeemVoucher db voucher fingerprint
      assertEqual "redeeming double-paid voucher" (Right ()) redeemResult
  ]


memoryDatabaseVoucherPaymentTests :: TestTree
memoryDatabaseVoucherPaymentTests = makeVoucherPaymentTests "memory" memory

sqlite3DatabaseVoucherPaymentTests :: TestTree
sqlite3DatabaseVoucherPaymentTests =
  makeVoucherPaymentTests "sqlite3" $
  do
    tempdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempdir "voucher-.db"
    getDBConnection $ Text.pack path
