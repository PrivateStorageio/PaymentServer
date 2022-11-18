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
  ( testCase )

import Data.Aeson
  ( encode
  )

import Network.Wai.Test
  ( SRequest(SRequest)
  , runSession
  , srequest
  , defaultRequest
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
  , payForVoucher
  )
import PaymentServer.Redemption
  ( Redeem(Redeem)
  , RedemptionConfig
    ( RedemptionConfig
    , redemptionConfigNumGroups
    , redemptionConfigTokensPerVoucher
    , redemptionConfigIssue
    )
  )
import PaymentServer.Server
  ( paymentServerApp
  )
import FakeStripe
  ( withFakeStripe
  , chargeOkay
  , ChargeId(ChargeId)
  )

import Control.Monad ( void )

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
    let redemption = Redeem aVoucher (replicate tokensPerGroup aToken) 0
    in assertRedemptionStatus redemption 200

  , testCase "negative counter" $
    -- A redemption attempt with a negative counter value fails with an HTTP
    -- error.
    let redemption = Redeem aVoucher (replicate tokensPerGroup aToken) (-1)
    in assertRedemptionStatus redemption 400

  , testCase "counter too large" $
    -- A redemption attempt with a counter value greater than the maximum
    -- allowed fails with an HTTP error.
    let redemption = Redeem aVoucher (replicate tokensPerGroup aToken) tokenGroups
    in assertRedemptionStatus redemption 400

  , testCase "too few tokens" $
    -- A redemption attempt with fewer tokens than are expected in the
    -- indicated redemption group fails with an HTTP error.
    let redemption = Redeem aVoucher (replicate (tokensPerGroup - 1) aToken) 0
    in assertRedemptionStatus redemption 400

  , testCase "too many tokens" $
    -- A redemption attempt with more tokens than are expected in the
    -- indicated redemption group fails with an HTTP error.
    let redemption = Redeem aVoucher (replicate (tokensPerGroup + 1) aToken) 0
    in assertRedemptionStatus redemption 400
  ]
  where
    totalTokens = 32
    tokenGroups = 4
    tokensPerGroup = totalTokens `div` tokenGroups
    aToken = "a"
    aVoucher = "abc"

    redemptionConfig = RedemptionConfig
      { redemptionConfigNumGroups = tokenGroups
      , redemptionConfigTokensPerVoucher = totalTokens
      , redemptionConfigIssue = trivialIssue
      }
    origins = ["example.invalid"]
    headers = [("origin", "example.invalid"), ("content-type", "application/json")]
    path = "/v1/redeem"
    theRequest = setPath defaultRequest
                 { requestMethod = "POST"
                 , requestHeaders = headers
                 } path

    request = srequest . SRequest theRequest . encode

    -- | Assert that using the given redemption parameters results in a
    -- response with the given status.
    assertRedemptionStatus redemption expectedStatus =
      withFakeStripe (return chargeOkay) $
      \webhookConfig stripeConfig -> do
        db <- memory
        void $ payForVoucher db aVoucher (return $ Right $ ChargeId "xyz")

        -- It would be nice if we exercised `getApp` here instead of doing it
        -- all ourselves.
        let app = paymentServerApp origins stripeConfig webhookConfig redemptionConfig db

        flip runSession app $ do
          response <- request redemption
          assertStatus expectedStatus response
