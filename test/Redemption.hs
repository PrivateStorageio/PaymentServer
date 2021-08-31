{-# LANGUAGE OverloadedStrings #-}

-- | Tests related to PaymentServer.Redemption.

module Redemption
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

import Data.Aeson
  ( encode
  )

import Network.Wai.Handler.Warp
  ( testWithApplication
  )

import Network.Wai.Test
  ( SRequest(SRequest)
  , runSession
  , request
  , srequest
  , defaultRequest
  , assertHeader
  , assertStatus
  , setPath
  )

import Network.Wai
  ( requestMethod
  , requestHeaders
  )

import PaymentServer.Issuer
  ( trivialIssue
  )

import PaymentServer.Persistence
  ( memory
  )

import PaymentServer.Server
  ( RedemptionConfig(RedemptionConfig)
  , paymentServerApp
  )
import PaymentServer.Redemption
  ( Redeem(Redeem)
  )

import FakeStripe
  ( withFakeStripe
  , chargeOkay
  )

tests :: TestTree
tests = testGroup "Redemption"
  [ redemptionTests
  ]

redemptionTests :: TestTree
redemptionTests =
  testGroup "voucher redemption"
  [ testCase "success" $
    -- A redemption attempt with a valid group number and correct token count
    -- for that group receives a 200 HTTP response.
    withFakeStripe (return chargeOkay) $
    \stripeConfig -> do
      db <- memory
      let app = paymentServerApp origins stripeConfig redemptionConfig db

      (flip runSession) app $ do
        response <- request $ Redeem "abc" (replicate (1024 `div` 16) "a") 0
        assertStatus 200 response

  , testCase "negative counter" $
    -- A redemption attempt with a negative counter value fails with an HTTP
    -- error.
    withFakeStripe (return chargeOkay) $
    \stripeConfig -> do
      db <- memory
      let app = paymentServerApp origins stripeConfig redemptionConfig db

      (flip runSession) app $ do
        response <- request $ Redeem "abc" (replicate (1024 `div` 16) "a") (-1)
        assertStatus 400 response
  ]
  where
    redemptionConfig = RedemptionConfig 16 1024 trivialIssue
    origins = ["example.invalid"]
    headers = [("origin", "example.invalid"), ("content-type", "application/json")]
    path = "/v1/redeem"
    theRequest = setPath defaultRequest
                 { requestMethod = "POST"
                 , requestHeaders = headers
                 } path

    request = srequest . SRequest theRequest . encode
