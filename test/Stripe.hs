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


import Control.Monad.IO.Class
  ( liftIO
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

import Network.Wai.Test
  ( SRequest(SRequest)
  , runSession
  , request
  , srequest
  , defaultRequest
  , assertHeader
  , setPath
  )

import Network.Wai
  ( requestMethod
  , requestHeaders
  )

import PaymentServer.Persistence
  ( memory
  )

import PaymentServer.Processors.Stripe
  ( Charges(Charges)
  , Acknowledgement(Ok)
  , charge

  )

import PaymentServer.Issuer
  ( trivialIssue
  )

import PaymentServer.Server
  ( paymentServerApp
  )

import FakeStripe
  ( withFakeStripe
  , chargeOkay
  )

tests :: TestTree
tests = testGroup "Stripe"
  [ chargeTests
  , corsTests
  ]

corsTests :: TestTree
corsTests =
  testGroup "CORS"
  [ testCase "a request with the wrong content-type receives a CORS-enabled response" $
    assertCORSHeader "POST" textPlain validChargeBytes
  , testCase "a request without a valid charge in the body receives a CORS-enabled response" $
    assertCORSHeader "POST" applicationJSON invalidChargeBytes
  , testCase "a request with the wrong request method receives a CORS-enabled response" $
    assertCORSHeader "GET" applicationJSON validChargeBytes
  , testCase "a request with a valid charge in the body receives a CORS-enabled response" $
    assertCORSHeader "POST" applicationJSON validChargeBytes
  ]
  where
    textPlain = [("content-type", "text/plain")]
    applicationJSON = [("content-type", "application/json")]
    validChargeBytes = "{\"token\": \"abcdef\", \"voucher\": \"lmnopqrst\", \"amount\": \"650\", \"currency\": \"USD\"}"
    invalidChargeBytes = "[1, 2, 3]"

    assertCORSHeader method headers body =
      withFakeStripe (return chargeOkay) $
      \stripeConfig -> do
        db <- memory
        let origins = ["example.invalid"]
        let app = paymentServerApp origins stripeConfig trivialIssue db

        let path = "/v1/stripe/charge"
        let theRequest = setPath defaultRequest { requestMethod = method, requestHeaders = headers} path
        let theSRequest = SRequest theRequest body
        (flip runSession) app $ do
          response <- srequest theSRequest
          liftIO $ print response
          assertHeader "Access-Control-Allow-Origin" "example.invalid" response


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
