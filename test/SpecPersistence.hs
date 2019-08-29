{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Test suite related to the persistence system.
--

module SpecPersistence where

import Test.QuickCheck
  ( Property
  , (==>)
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.Hspec.Expectations
  ( shouldReturn
  )
import Test.QuickCheck
  ( property
  )
import Test.QuickCheck.Monadic
  ( monadicIO
  , run
  , assert
  , pre
  )
import Test.QuickCheck.Instances.Text
  (
  )
import PaymentServer.Persistence
  ( RedeemError(NotPaid, AlreadyRedeemed)
  , Voucher
  , Fingerprint
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , memory
  )

-- | A voucher which has not been paid for cannot be redeemed.
unpaidVoucherNotRedeemable :: VoucherDatabase d => IO d -> Property
unpaidVoucherNotRedeemable getDB = property $ \voucher fingerprint -> do
  db <- liftIO getDB
  redeemVoucher db voucher fingerprint `shouldReturn` Left NotPaid

-- | A voucher which is paid for can be redeemed with any fingerprint.
paidVoucherRedeemable :: VoucherDatabase d => IO d -> Property
paidVoucherRedeemable getDB = property $ \voucher fingerprint -> do
  db <- liftIO getDB
  () <- payForVoucher db voucher
  redeemVoucher db voucher fingerprint `shouldReturn` Right ()

-- | A voucher which is paid for can be redeemed more than once as long as the
-- same fingerprint is used each time.
paidVoucherMultiRedeemable :: VoucherDatabase d => IO d -> Property
paidVoucherMultiRedeemable getDB = property $ \voucher fingerprint -> do
  db <- liftIO getDB
  () <- payForVoucher db voucher
  let redeem = redeemVoucher db voucher fingerprint
  redeem
  redeem `shouldReturn` Right ()

-- | A voucher which is paid for can not be redeemed a second time with a
-- different fingerprint than was used on the first attempt.
paidVoucherMismatchFingerprint :: VoucherDatabase d => IO d -> Property
paidVoucherMismatchFingerprint getDB = property $ \voucher fingerprint fingerprint' ->
  fingerprint /= fingerprint' ==> do
  db <- liftIO getDB
  () <- payForVoucher db voucher
  let redeem = redeemVoucher db voucher
  redeem fingerprint
  redeem fingerprint' `shouldReturn` Left AlreadyRedeemed

makeSpec :: VoucherDatabase d => IO d -> Spec
makeSpec getDB = do
  describe "voucher interactions" $ do
    it "denies redemption of a not-paid-for voucher" $ unpaidVoucherNotRedeemable getDB
    it "allows redemption of paid-for vouchers" $ paidVoucherRedeemable getDB
    it "allows multiple redemption as long as the same fingerprint is used" $ paidVoucherMultiRedeemable getDB
    it "denies a subsequent redemption with a different fingerprint" $ paidVoucherMismatchFingerprint getDB

spec_memory = makeSpec memory
