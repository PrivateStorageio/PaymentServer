{-# LANGUAGE OverloadedStrings #-}

-- | Tests related to PaymentServer.Processors.Stripe.

module Stripe
  ( tests
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  , assertEqual
  )


import Control.Monad.Trans.Except
  ( runExceptT
  )

import Servant.Server
  ( Handler(runHandler')
  , ServerError(ServerError)
  )

import Web.Stripe.Types
  ( Currency(USD, AED)
  )

import PaymentServer.Persistence
  ( memory
  )

import PaymentServer.Processors.Stripe
  ( Charges(Charges)
  , Acknowledgement(Ok)
  , charge

  )

import FakeStripe
  ( withFakeStripe
  , chargeOkay
  )

tests :: TestTree
tests = testGroup "Stripe"
  [ chargeTests
  ]

chargeTests :: TestTree
chargeTests =
  testGroup "Charges"
  [ testCase "non-USD currency is rejected" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 650
      let currency = AED
      db <- memory
      (Left (ServerError code _ _ _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
  , testCase "incorrect USD amount is rejected" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 649
      let currency = USD
      db <- memory
      (Left (ServerError code _ _ _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
  , testCase "currect USD amount is accepted" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      result <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is Ok" (Right Ok) result
  ]
  where
    token = "foo"
    voucher = "bar"
