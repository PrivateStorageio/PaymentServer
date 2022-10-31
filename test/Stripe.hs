{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests related to PaymentServer.Processors.Stripe.

module Stripe
  ( tests
  ) where

import Prelude hiding
  ( concat
  )

import Text.RawString.QQ
  ( r
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
  ( liftIO
  )

import Control.Monad.Trans.Except
  ( runExceptT
  )

import Stripe.Concepts
  ( WebhookSecretKey(WebhookSecretKey)
  )

import Servant.Server
  ( Handler(runHandler')
  , ServerError(ServerError)
  )

import Data.Aeson
  ( decode
  , encode
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
  , SResponse(simpleStatus, simpleBody)
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
  , RedeemError(NotPaid)
  , memory
  , payForVoucher
  , redeemVoucher
  )

import PaymentServer.Processors.Stripe
  ( Charges(Charges)
  , Acknowledgement(Ok)
  , Failure(Failure)
  , WebhookConfig(WebhookConfig)
  , charge
  , webhookServer
  , stripeSignature
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
      \webhookConfig stripeConfig -> do
        let origins = ["example.invalid"]
        let redemptionConfig = RedemptionConfig 16 1024 trivialIssue
        let app = paymentServerApp origins stripeConfig webhookConfig redemptionConfig db

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
    withFakeStripe (return chargeOkay) $ \webhookConfig stripeConfig -> do
      let amount = 650
      let currency = AED
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The HTTP phrase matches the code" "Bad Request" phrase
      assertEqual "The JSON body includes the reason" (Just $ Failure "Unsupported currency") (decode body)

  , testCase "incorrect USD amount is rejected" $
    withFakeStripe (return chargeOkay) $ \webhookConfig stripeConfig -> do
      let amount = 649
      let currency = USD
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is an error" 400 code
      assertEqual "The HTTP phrase matches the code" "Bad Request" phrase
      assertEqual "The JSON body includes the reason" (Just $ Failure "Incorrect charge amount") (decode body)

  , testCase "a Stripe charge failure is propagated" $
    withFakeStripe (return (chargeFailed cardError)) $ \webhookConfig stripeConfig -> do
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
    withFakeStripe (return (chargeFailed apiError)) $ \webhookConfig stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      (Left (ServerError code phrase body _)) <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      -- The `apiError` is for a Stripe API error.
      assertEqual "The result is an error" 503 code
      assertEqual "The HTTP phrase matches the code" "Service Unavailable" phrase

  , testCase "currect USD amount is accepted" $
    withFakeStripe (return chargeOkay) $ \webhookConfig stripeConfig -> do
      let amount = 650
      let currency = USD
      db <- memory
      result <- runExceptT . runHandler' $ charge stripeConfig db (Charges token voucher amount currency)
      assertEqual "The result is Ok" (Right Ok) result
  ]
  where
    token = "foo"
    voucher = "bar"

webhookTests :: TestTree
webhookTests =
  testGroup "The Stripe charge web hook"
  [ testCase "If the signature is missing then the response is Bad Request" $ do
      db <- memory

      let
        theSRequest = SRequest jsonRequest checkoutSessionCompleted
        app = makeApp db

      response <- (flip runSession) app $ srequest theSRequest
      assertEqual "The body reflects the error" (Just $ Failure "missing signature") (decode . simpleBody $ response)
      assertEqual "The response is 400" status400 (simpleStatus response)
      assertNotRedeemable db voucher fingerprint

  , testCase "If the signature is misformatted then the response is Bad Request" $ do
      db <- memory
      let
        app = makeApp db
        theRequest = signedRequest "Do you like my signature?"
        theSRequest = SRequest theRequest checkoutSessionCompleted

      response <- (flip runSession) app $ srequest theSRequest
      assertEqual "The body reflects the error" (Just $ Failure "malformed signature") (decode . simpleBody $ response)
      assertEqual "The response is 400" status400 (simpleStatus response)
      assertNotRedeemable db voucher fingerprint

  , testCase "If the signature is incorrect then no attempt is made to parse the request body and the response is Bad Request" $ do
      db <- memory
      let
        app = makeApp db
        theRequest = signedRequest $ stripeSignature (WebhookSecretKey "key") timestamp "Some other body"
        theSRequest = SRequest theRequest checkoutSessionCompleted

      response <- (flip runSession) app $ srequest theSRequest
      assertEqual "The body reflects the error" (Just $ Failure "invalid signature") (decode . simpleBody $ response)
      assertEqual "The response is 400" status400 (simpleStatus response)
      assertNotRedeemable db voucher fingerprint

  , testCase "If the signature is correct and the body is not JSON then the response is Bad Request" $ do
      db <- memory
      let
        nonJSONBody = "Some other body"
        app = makeApp db
        theRequest = signedRequest $ stripeSignature webhookSecret timestamp nonJSONBody
        theSRequest = SRequest theRequest (LBS.fromStrict nonJSONBody)

      response <- (flip runSession) app $ srequest theSRequest

      -- It should fail but we don't really care what the message is.
      let (Just (Failure _)) = decode . simpleBody $ response
      assertEqual "The response is 400" status400 (simpleStatus response)

  , testCase "If the request body contains a checkout.session.completed event and the signature is correct then the voucher is marked as paid and the response is OK" $ do
      db <- runRequest checkoutSessionCompleted >>= assertOkResponse
      -- It has been paid so we should be allowed to redeem it.
      assertRedeemable db voucher fingerprint

  , testCase "The response to any other event is Bad Request" $
      runRequest productCreated >>= assertResponse status400

  ]
  where
    runRequest body = do
      db <- memory
      let
        app = makeApp db
        theRequest = (flip setPath) path defaultRequest
                     { requestMethod = "POST"
                     , requestHeaders = [ ("content-type", "application/json; charset=utf-8")
                                        , ("Stripe-Signature", stripeSignature webhookSecret timestamp (LBS.toStrict body))
                                        ]
                     }
        theSRequest = SRequest theRequest body

      response <- (flip runSession) app $ srequest theSRequest
      return (db, response)

    -- Assert that the response to a correctly signed applicaton/json request
    -- with the given body is 200 OK.
    assertOkResponse (db, response) = do
      assertEqual "The body reflects success" (encode Ok) (simpleBody response)
      assertResponse status200 (db, response)
      return db

    assertResponse status (db, response) =
      assertEqual ("The response is " ++ (show status)) status (simpleStatus response)

    -- Assert that the database allows us to redeem a voucher, demonstrating
    -- that the voucher has persistent state consistent with payment having
    -- been received.
    assertRedeemable db voucher fingerprint = do
      redeemed <- redeemVoucher db voucher fingerprint
      assertEqual "The voucher is redeemable." (Right True) redeemed

    -- Assert the opposite of assertRedeemable
    assertNotRedeemable db voucher fingerprint = do
      redeemed <- redeemVoucher db voucher fingerprint
      assertEqual "The unpaid voucher is not redeemable." (Left NotPaid) redeemed

    makeApp = paymentServerApp origins stripeConfig webhookConfig redemptionConfig

    -- Arbitrary strings that don't matter apart from how they compare to
    -- other values in the same range.  Maybe Voucher and Fingerprint should
    -- be newtype instead of type.  Note that the voucher value does appear in
    -- the checkoutSessionCompleted value below, though.
    voucher = "abcdefghi"
    fingerprint = "rstuvwxyz"

    timestamp = 1234567890

    keyBytes = "an extremely good key"
    stripeKey = StripeKey keyBytes
    stripeConfig = StripeConfig stripeKey Nothing
    webhookSecretBytes = "very secret bytes"
    webhookSecret = WebhookSecretKey webhookSecretBytes
    webhookConfig = WebhookConfig webhookSecret
    origins = []
    redemptionConfig = RedemptionConfig 16 1024 trivialIssue

    -- The path at which our server exposes the Stripe webhook handler.
    path = "/v1/stripe/webhook"

    -- Some request values useful for the various cases we want to test.
    postRequest = (flip setPath) path defaultRequest
      { requestMethod = "POST"
      }

    jsonRequest = postRequest
      { requestHeaders = [("content-type", "application/json; charset=utf-8")]
      }

    signedRequest sig = jsonRequest
      { requestHeaders = ("Stripe-Signature", sig):requestHeaders jsonRequest
      }

-- Note the client_reference_id contained within matches the voucher defined
-- above.
checkoutSessionCompleted :: LBS.ByteString
checkoutSessionCompleted = [r|
{
  "id": "evt_1LxcsdBHXBAMm9bPSq6UWAZe",
  "object": "event",
  "api_version": "2019-11-05",
  "created": 1666903247,
  "data": {
    "object": {
      "id": "cs_test_a1kWLWGoXZPa6ywyVnuib8DPA3BqXCWZX5UEjLfKh7gLjdZy2LD3F5mEp3",
      "object": "checkout.session",
      "after_expiration": null,
      "allow_promotion_codes": null,
      "amount_subtotal": 3000,
      "amount_total": 3000,
      "automatic_tax": {
        "enabled": false,
        "status": null
      },
      "billing_address_collection": null,
      "cancel_url": "https://httpbin.org/post",
      "client_reference_id": "abcdefghi",
      "consent": null,
      "consent_collection": null,
      "created": 1666903243,
      "currency": "usd",
      "customer": "cus_Mh0u62xtelUehD",
      "customer_creation": "always",
      "customer_details": {
        "address": {
          "city": null,
          "country": null,
          "line1": null,
          "line2": null,
          "postal_code": null,
          "state": null
        },
        "email": "stripe@example.com",
        "name": null,
        "phone": null,
        "tax_exempt": "none",
        "tax_ids": [

        ]
      },
      "customer_email": null,
      "display_items": [
        {
          "amount": 1500,
          "currency": "usd",
          "custom": {
            "description": "comfortable cotton t-shirt",
            "images": null,
            "name": "t-shirt"
          },
          "quantity": 2,
          "type": "custom"
        }
      ],
      "expires_at": 1666989643,
      "livemode": false,
      "locale": null,
      "metadata": {
      },
      "mode": "payment",
      "payment_intent": "pi_3LxcsZBHXBAMm9bP1daBGoPV",
      "payment_link": null,
      "payment_method_collection": "always",
      "payment_method_options": {
      },
      "payment_method_types": [
        "card"
      ],
      "payment_status": "paid",
      "phone_number_collection": {
        "enabled": false
      },
      "recovered_from": null,
      "setup_intent": null,
      "shipping": null,
      "shipping_address_collection": null,
      "shipping_options": [

      ],
      "shipping_rate": null,
      "status": "complete",
      "submit_type": null,
      "subscription": null,
      "success_url": "https://httpbin.org/post",
      "total_details": {
        "amount_discount": 0,
        "amount_shipping": 0,
        "amount_tax": 0
      },
      "url": null
    }
  },
  "livemode": false,
  "pending_webhooks": 2,
  "request": {
    "id": null,
    "idempotency_key": null
  },
  "type": "checkout.session.completed"
}
|]


productCreated :: LBS.ByteString
productCreated = [r|
{
  "id": "evt_1LyxesBHXBAMm9bPqekQW4Yj",
  "object": "event",
  "api_version": "2019-11-05",
  "created": 1667221446,
  "data": {
    "object": {
      "id": "prod_MiOR6hX1zcaGfJ",
      "object": "product",
      "active": true,
      "attributes": [

      ],
      "created": 1667221445,
      "default_price": null,
      "description": "(created by Stripe CLI)",
      "images": [

      ],
      "livemode": false,
      "metadata": {
      },
      "name": "myproduct",
      "package_dimensions": null,
      "shippable": null,
      "statement_descriptor": null,
      "tax_code": null,
      "type": "service",
      "unit_label": null,
      "updated": 1667221446,
      "url": null
    }
  },
  "livemode": false,
  "pending_webhooks": 2,
  "request": {
    "id": "req_kvFraITogK8pZB",
    "idempotency_key": "74150cd6-6ac5-4144-859f-4e6774adb09d"
  },
  "type": "product.created"
}
|]
