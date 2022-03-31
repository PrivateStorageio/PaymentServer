{-# LANGUAGE OverloadedStrings #-}

-- | Tests related to PaymentServer.Processors.Stripe.

module Stripe
  ( tests
  ) where

import Prelude hiding
  ( concat
  )

import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  , assertEqual
  )


import Data.Text.Lazy.Encoding
  ( encodeUtf8
  )
import Data.Text.Lazy
  ( Text
  , toStrict
  , concat
  )

import qualified Data.ByteString.Lazy as LBS
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

import Data.Aeson
  ( decode
  )

import Web.Stripe.Types
  ( Currency(USD, AED)
  , ChargeId(ChargeId)
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
  ( Voucher
  , memory
  , payForVoucher
  )

import PaymentServer.Processors.Stripe
  ( Charges(Charges)
  , Acknowledgement(Ok)
  , Failure(Failure)
  , charge

  )

import PaymentServer.Issuer
  ( trivialIssue
  )

import PaymentServer.Server
  ( RedemptionConfig(RedemptionConfig)
  , paymentServerApp
  )

import FakeStripe
  ( withFakeStripe
  , chargeOkay
  , chargeFailed
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
    assertCORSHeader chargeOkay "POST" textPlain validChargeBytes

  , testCase "a request without a valid charge in the body receives a CORS-enabled response" $
    assertCORSHeader chargeOkay "POST" applicationJSON invalidChargeBytes

  , testCase "a request with the wrong request method receives a CORS-enabled response" $
    assertCORSHeader chargeOkay "GET" applicationJSON validChargeBytes

  , testCase "a request associated with an error from Stripe receives a CORS-enabled response" $
    assertCORSHeader chargeFailed "POST" applicationJSON validChargeBytes

  , testCase "a request with a valid charge in the body receives a CORS-enabled response" $
    assertCORSHeader chargeOkay "POST" applicationJSON validChargeBytes

  , testCase "a request with an already-paid voucher receives a CORS-enabled response" $ do
      let pay = return . Right . ChargeId $ "abc"
      db <- memory
      payForVoucher db (toStrict alreadyPaidVoucher') pay
      assertCORSHeader' db chargeOkay "POST" applicationJSON (alreadyPaidVoucher alreadyPaidVoucher')
  ]
  where
    textPlain = [("content-type", "text/plain")]
    applicationJSON = [("content-type", "application/json")]
    validChargeBytes = "{\"token\": \"abcdef\", \"voucher\": \"lmnopqrst\", \"amount\": \"650\", \"currency\": \"USD\"}"
    invalidChargeBytes = "[1, 2, 3]"

    alreadyPaidVoucher' :: Text
    alreadyPaidVoucher' = "hello world"

    alreadyPaidVoucher :: Text -> LBS.ByteString
    alreadyPaidVoucher voucher = encodeUtf8 $ concat ["{\"token\": \"abcdef\", \"voucher\": \"", voucher, "\", \"amount\": \"650\", \"currency\": \"USD\"}"]

    assertCORSHeader stripeResponse method headers body = do
      db <- memory
      assertCORSHeader' db stripeResponse method headers body

    assertCORSHeader' db stripeResponse method headers body =
      withFakeStripe (return stripeResponse) $
      \stripeConfig -> do
        let origins = ["example.invalid"]
        let redemptionConfig = RedemptionConfig 16 1024 trivialIssue
        let app = paymentServerApp origins stripeConfig redemptionConfig db

        let path = "/v1/stripe/charge"
        let theRequest = setPath defaultRequest
              { requestMethod = method
              , requestHeaders = ("origin", "example.invalid"):headers
              } path
        let theSRequest = SRequest theRequest body
        (flip runSession) app $ do
          response <- srequest theSRequest
          assertHeader "Access-Control-Allow-Origin" "example.invalid" response


chargeTests :: TestTree
chargeTests =
  testGroup "Charges"
  [ testCase "non-USD currency is rejected" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 650
      let currency = AED
      db <- memory
      (Left (ServerError code _ body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The JSON body includes the reason" (Just $ Failure "Unsupported currency") (decode body)
  , testCase "incorrect USD amount is rejected" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 649
      let currency = USD
      db <- memory
      (Left (ServerError code _ body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The JSON body includes the reason" (Just $ Failure "Incorrect charge amount") (decode body)
  , testCase "a Stripe charge failure is propagated" $
    withFakeStripe (return chargeFailed) $ \stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      (Left (ServerError code _ body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      -- The chargeFailed fixture is hard-coded for a card expired error.
      assertEqual "The JSON body includes the reason"
        (Just $ Failure "Stripe charge didn't succeed: Your card is expired.") (decode body)
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
