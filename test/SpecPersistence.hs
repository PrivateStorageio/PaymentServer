{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Test suite related to the persistence system.
--

module SpecPersistence where

import Test.QuickCheck
  ( Property
  , (===)
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Test.QuickCheck.Monadic
  ( monadicIO
  , run
  , assert
  , pre
  )
import PaymentServer.Persistence
  ( RedeemError(NotPaid, AlreadyRedeemed)
  , Voucher
  , Fingerprint
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , memory
  )

-- | A voucher which has not been paid for cannot be redeemed.
unpaidVoucherNotRedeemable :: VoucherDatabase d => IO d -> Voucher -> Fingerprint -> Property
unpaidVoucherNotRedeemable getDB voucher fingerprint = monadicIO $ do
  db <- liftIO getDB
  result <- run $ redeemVoucher db voucher fingerprint
  assert (result == Left NotPaid)

-- | The in-memory implementation for unpaidVoucherNotRedeemable.
prop_memory_unpaidVoucherNotRedeemable = unpaidVoucherNotRedeemable memory

-- | A voucher which is paid for can be redeemed with any fingerprint.
paidVoucherRedeemable :: VoucherDatabase d => IO d -> Voucher -> Fingerprint -> Property
paidVoucherRedeemable getDB voucher fingerprint = monadicIO $ do
  db <- liftIO getDB
  () <- run $ payForVoucher db voucher
  result <- run $ redeemVoucher db voucher fingerprint
  assert (result == Right ())

-- | The in-memory implementation for paidVoucherRedeemable.
prop_memory_paidVoucherRedeemable = paidVoucherRedeemable memory

-- | A voucher which is paid for can be redeemed more than once as long as the
-- same fingerprint is used each time.
paidVoucherMultiRedeemable :: VoucherDatabase d => IO d -> Voucher -> Fingerprint -> Property
paidVoucherMultiRedeemable getDB voucher fingerprint = monadicIO $ do
  db <- liftIO getDB
  () <- run $ payForVoucher db voucher
  let redeem = redeemVoucher db voucher fingerprint
  run redeem
  result <- run redeem
  assert (result == Right ())

-- | The in-memory implementation for paidVoucherMultiRedeemable.
prop_memory_paidVoucherMultiRedeemable = paidVoucherMultiRedeemable memory

-- | A voucher which is paid for can not be redeemed a second time with a
-- different fingerprint than was used on the first attempt.
paidVoucherMismatchFingerprint :: VoucherDatabase d => IO d -> Voucher -> Fingerprint -> Fingerprint -> Property
paidVoucherMismatchFingerprint getDB voucher fingerprint fingerprint' = monadicIO $ do
  pre (fingerprint /= fingerprint')
  db <- liftIO getDB
  () <- run $ payForVoucher db voucher
  let redeem = redeemVoucher db voucher
  run $ redeem fingerprint
  result <- run $ redeem fingerprint'
  assert (result == Left AlreadyRedeemed)

-- | The in-memory implementation for paidVoucherMismatchFingerprint.
prop_memory_paidVoucherMismatchFingerprint = paidVoucherMismatchFingerprint memory
