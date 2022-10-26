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

import Web.Stripe.Client
  ( StripeConfig(StripeConfig)
  , StripeKey(StripeKey)
  )

import Web.Stripe.Types
  ( Currency(USD, AED)
  , ChargeId(ChargeId)
  )
import Network.HTTP.Types
  ( status200
  , status400
  )
import Network.Wai.Test
  ( SRequest(SRequest)
  , SResponse(simpleStatus)
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
  , webhookServer
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
  , cardError
  , apiError
  )

tests :: TestTree
tests = testGroup "Stripe"
  [ chargeTests
  , corsTests
  , webhookTests
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
    assertCORSHeader (chargeFailed cardError) "POST" applicationJSON validChargeBytes

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
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The HTTP phrase matches the code" "Bad Request" phrase
      assertEqual "The JSON body includes the reason" (Just $ Failure "Unsupported currency") (decode body)

  , testCase "incorrect USD amount is rejected" $
    withFakeStripe (return chargeOkay) $ \stripeConfig -> do
      let amount = 649
      let currency = USD
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The HTTP phrase matches the code" "Bad Request" phrase
      assertEqual "The JSON body includes the reason" (Just $ Failure "Incorrect charge amount") (decode body)

  , testCase "a Stripe charge failure is propagated" $
    withFakeStripe (return (chargeFailed cardError)) $ \stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The HTTP phrase matches the code" "Bad Request" phrase
      -- The `cardError` is for a card expired error.
      assertEqual "The JSON body includes the reason"
        (Just $ Failure "Stripe charge didn't succeed: Your card is expired.") (decode body)

  , testCase "the HTTP error code is derived from the specific failure" $
    withFakeStripe (return (chargeFailed apiError)) $ \stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      -- The `apiError` is for a Stripe API error.
      assertEqual "The result is an error" 503 code
      assertEqual "The HTTP phrase matches the code" "Service Unavailable" phrase

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

-- TODO
-- Make "charge.succeeded" fail with a good error message
-- Make "charge.succeeded" pass
-- Prevent replay attacks? https://stripe.com/docs/webhooks/signatures#replay-attacks
-- Check network origin? https://stripe.com/docs/ips#webhook-notifications
-- Check the Stripe signature


webhookTests :: TestTree
webhookTests =
  testGroup "The Stripe charge web hook"
  [ testCase "If the signature header is missing then the response is a Bad Request" $ do
      db <- memory

      let
        theRequest = setPath defaultRequest
                     { requestMethod = "POST"
                     , requestHeaders = [("content-type", "application/json; charset=utf-8")]
                     } path
        theSRequest = SRequest theRequest body
        app = paymentServerApp origins stripeConfig redemptionConfig db

      response <- (flip runSession) app $ srequest theSRequest
      assertEqual "The response is 400" status400 (simpleStatus response)

  , testCase "If the signature header contains an invalid signature then the response is a Bad Request" $ do
      db <- memory
      let
        app = paymentServerApp origins stripeConfig redemptionConfig db
        theRequest = (flip setPath) path defaultRequest
                     { requestMethod = "POST"
                     , requestHeaders = [ ("content-type", "application/json; charset=utf-8")
                                        , ("HTTP_STRIPE_SIGNATURE", "Do you like my signature?")
                                        ]
                     }
        theSRequest = SRequest theRequest body

      response <- (flip runSession) app $ srequest theSRequest
      assertEqual "The response is 400" status400 (simpleStatus response)

  ]
  where
    stripeKey = StripeKey ""
    stripeConfig = StripeConfig stripeKey Nothing
    origins = []
    redemptionConfig = RedemptionConfig 16 1024 trivialIssue
    path = "/v1/stripe/webhook"
    body = "{\"api_version\":\"2022-08-01\",\"created\":1665593127,\"data\":{\"object\":{\"amount\":250,\"amount_captured\":250,\"amount_refunded\":0,\"application\":null,\"application_fee\":null,\"application_fee_amount\":null,\"balance_transaction\":\"txn_3Ls83eLswFpehDNg0dFvPaKv\",\"billing_details\":{\"address\":{\"city\":null,\"country\":\"DE\",\"line1\":null,\"line2\":null,\"postal_code\":null,\"state\":null},\"email\":\"a@b.d\",\"name\":\"asdfasf\",\"phone\":null},\"calculated_statement_descriptor\":\"Stripe\",\"captured\":true,\"created\":1665593127,\"currency\":\"usd\",\"customer\":null,\"description\":null,\"destination\":null,\"dispute\":null,\"disputed\":false,\"failure_balance_transaction\":null,\"failure_code\":null,\"failure_message\":null,\"fraud_details\":{},\"id\":\"ch_3Ls83eLswFpehDNg0WVw0vTa\",\"invoice\":null,\"livemode\":false,\"metadata\":{},\"object\":\"charge\",\"on_behalf_of\":null,\"order\":null,\"outcome\":{\"network_status\":\"approved_by_network\",\"reason\":null,\"risk_level\":\"normal\",\"risk_score\":21,\"seller_message\":\"Payment complete.\",\"type\":\"authorized\"},\"paid\":true,\"payment_intent\":\"pi_3Ls83eLswFpehDNg0b2mAFUW\",\"payment_method\":\"pm_1Ls83dLswFpehDNgpYAGL3j9\",\"payment_method_details\":{\"card\":{\"brand\":\"mastercard\",\"checks\":{\"address_line1_check\":null,\"address_postal_code_check\":null,\"cvc_check\":\"pass\"},\"country\":\"US\",\"exp_month\":12,\"exp_year\":2023,\"fingerprint\":\"DoAWRfUcyOfJupbL\",\"funding\":\"credit\",\"installments\":null,\"last4\":\"4444\",\"mandate\":null,\"network\":\"mastercard\",\"three_d_secure\":null,\"wallet\":null},\"type\":\"card\"},\"receipt_email\":null,\"receipt_number\":null,\"receipt_url\":\"https://pay.stripe.com/receipts/payment/CAcaFwoVYWNjdF8xTGZORGFMc3dGcGVoRE5nKKjem5oGMgZo4m-xDMM6LBadftys-t7FIeo23hfQKTAtYI3zpLwmJb_3-A6VqCpIGjfmpkWUwCDQC38M\",\"refunded\":false,\"refunds\":{\"data\":[],\"has_more\":false,\"object\":\"list\",\"total_count\":0,\"url\":\"/v1/charges/ch_3Ls83eLswFpehDNg0WVw0vTa/refunds\"},\"review\":null,\"shipping\":null,\"source\":null,\"source_transfer\":null,\"statement_descriptor\":null,\"statement_descriptor_suffix\":null,\"status\":\"succeeded\",\"transfer_data\":null,\"transfer_group\":null}},\"id\":\"evt_3Ls83eLswFpehDNg0dmzogyf\",\"livemode\":false,\"object\":\"event\",\"pending_webhooks\":2,\"request\":{\"id\":\"req_F8pjOORr12gJT9\",\"idempotency_key\":\"8fdd25c9-cb73-4807-973f-f0b21d8bb7cc\"},\"type\":\"charge.succeeded\"}"
