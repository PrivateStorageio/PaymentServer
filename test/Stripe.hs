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

import Stripe.Signature
  ( digest
  , natBytes
  )

import qualified Data.ByteString.Base16 as Base16

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

  , testCase "If the signature is correct then the response is OK" $ do
      db <- assertOkResponse checkoutSessionCompleted
      -- It has been paid so we should be allowed to redeem it.
      assertRedeemable db voucher fingerprint

  , testCase "The response to a charge.succeeded is OK" $ do
      db <- assertOkResponse chargeSucceeded
      -- It is only redeemable after checkout.session.completed.
      assertNotRedeemable db voucher fingerprint

  , testCase "The response to a payment_intent.created is OK" $ do
      db <- assertOkResponse paymentIntentCreated
      -- It is only redeemable after checkout.session.completed.
      assertNotRedeemable db voucher fingerprint

  , testCase "The response to a customer.created is OK" $ do
      db <- assertOkResponse customerCreated
      -- It is only redeemable after checkout.session.completed.
      assertNotRedeemable db voucher fingerprint
  ]
  where
    -- Assert that the response to a correctly signed applicaton/json request
    -- with the given body is 200 OK.
    assertOkResponse body = do
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
      assertEqual "The body reflects success" (encode Ok) (simpleBody response)
      assertEqual "The response is 200" status200 (simpleStatus response)
      return db

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

    stripeSignature key when what = BS.concat
      [ "t="
      , natBytes when
      , ","
      , "v1="
      , encodeHex $ digest key when what
      ]

    timestamp = 1234567890
    encodeHex = Base16.encode

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

chargeSucceeded :: LBS.ByteString
chargeSucceeded = [r|
{
  "id": "evt_3LxcbqBHXBAMm9bP1XpbOJrq",
  "object": "event",
  "api_version": "2019-11-05",
  "created": 1666902207,
  "data": {
    "object": {
      "id": "ch_3LxcbqBHXBAMm9bP1QIFhXee",
      "object": "charge",
      "amount": 100,
      "amount_captured": 100,
      "amount_refunded": 0,
      "application": null,
      "application_fee": null,
      "application_fee_amount": null,
      "balance_transaction": "txn_3LxcbqBHXBAMm9bP1Q0skk4e",
      "billing_details": {
        "address": {
          "city": null,
          "country": null,
          "line1": null,
          "line2": null,
          "postal_code": null,
          "state": null
        },
        "email": null,
        "name": null,
        "phone": null
      },
      "calculated_statement_descriptor": "PRIVATESTORAGE.IO",
      "captured": true,
      "created": 1666902206,
      "currency": "usd",
      "customer": null,
      "description": "(created by Stripe CLI)",
      "destination": null,
      "dispute": null,
      "disputed": false,
      "failure_balance_transaction": null,
      "failure_code": null,
      "failure_message": null,
      "fraud_details": {
      },
      "invoice": null,
      "livemode": false,
      "metadata": {
      },
      "on_behalf_of": null,
      "order": null,
      "outcome": {
        "network_status": "approved_by_network",
        "reason": null,
        "risk_level": "normal",
        "risk_score": 35,
        "seller_message": "Payment complete.",
        "type": "authorized"
      },
      "paid": true,
      "payment_intent": null,
      "payment_method": "card_1LxcbqBHXBAMm9bPRIob1C1S",
      "payment_method_details": {
        "card": {
          "brand": "visa",
          "checks": {
            "address_line1_check": null,
            "address_postal_code_check": null,
            "cvc_check": null
          },
          "country": "US",
          "exp_month": 10,
          "exp_year": 2023,
          "fingerprint": "gLKhmoQYfsr1qGDi",
          "funding": "credit",
          "installments": null,
          "last4": "4242",
          "mandate": null,
          "network": "visa",
          "three_d_secure": null,
          "wallet": null
        },
        "type": "card"
      },
      "receipt_email": null,
      "receipt_number": null,
      "receipt_url": "https://pay.stripe.com/receipts/payment/CAcaFwoVYWNjdF8xRmhoeFRCSFhCQU1tOWJQKL_R65oGMgalIgGPgQc6LBaD9Kdq4Rg0Iz82re-NgTxpvigBVa_0K9HB7KHKy2v5eLI-3zt8J7kJZeRs",
      "refunded": false,
      "refunds": {
        "object": "list",
        "data": [

        ],
        "has_more": false,
        "total_count": 0,
        "url": "/v1/charges/ch_3LxcbqBHXBAMm9bP1QIFhXee/refunds"
      },
      "review": null,
      "shipping": null,
      "source": {
        "id": "card_1LxcbqBHXBAMm9bPRIob1C1S",
        "object": "card",
        "address_city": null,
        "address_country": null,
        "address_line1": null,
        "address_line1_check": null,
        "address_line2": null,
        "address_state": null,
        "address_zip": null,
        "address_zip_check": null,
        "brand": "Visa",
        "country": "US",
        "customer": null,
        "cvc_check": null,
        "dynamic_last4": null,
        "exp_month": 10,
        "exp_year": 2023,
        "fingerprint": "gLKhmoQYfsr1qGDi",
        "funding": "credit",
        "last4": "4242",
        "metadata": {
        },
        "name": null,
        "tokenization_method": null
      },
      "source_transfer": null,
      "statement_descriptor": null,
      "statement_descriptor_suffix": null,
      "status": "succeeded",
      "transfer_data": null,
      "transfer_group": null
    }
  },
  "livemode": false,
  "pending_webhooks": 2,
  "request": {
    "id": "req_u9SZdDchrHT1Iv",
    "idempotency_key": "2591ca44-b3b5-463b-b3ad-128bf954acfb"
  },
  "type": "charge.succeeded"
}
|]

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

paymentIntentCreated :: LBS.ByteString
paymentIntentCreated = [r|
{
  "id": "evt_3LxcZvBHXBAMm9bP1vttzzH9",
  "object": "event",
  "api_version": "2019-11-05",
  "created": 1666902087,
  "data": {
    "object": {
      "id": "pi_3LxcZvBHXBAMm9bP1eIHeoyO",
      "object": "payment_intent",
      "amount": 3000,
      "amount_capturable": 0,
      "amount_details": {
        "tip": {
        }
      },
      "amount_received": 0,
      "application": null,
      "application_fee_amount": null,
      "automatic_payment_methods": null,
      "canceled_at": null,
      "cancellation_reason": null,
      "capture_method": "automatic",
      "charges": {
        "object": "list",
        "data": [

        ],
        "has_more": false,
        "total_count": 0,
        "url": "/v1/charges?payment_intent=pi_3LxcZvBHXBAMm9bP1eIHeoyO"
      },
      "client_secret": "pi_3LxcZvBHXBAMm9bP1eIHeoyO_secret_diVUyvF9D65M4h8Azbr2j4kEA",
      "confirmation_method": "automatic",
      "created": 1666902087,
      "currency": "usd",
      "customer": null,
      "description": null,
      "invoice": null,
      "last_payment_error": null,
      "livemode": false,
      "metadata": {
      },
      "next_action": null,
      "on_behalf_of": null,
      "payment_method": null,
      "payment_method_options": {
        "card": {
          "installments": null,
          "mandate_options": null,
          "network": null,
          "request_three_d_secure": "automatic"
        }
      },
      "payment_method_types": [
        "card"
      ],
      "processing": null,
      "receipt_email": null,
      "review": null,
      "setup_future_usage": null,
      "shipping": {
        "address": {
          "city": "townsville",
          "country": "US",
          "line1": "123 Street road",
          "line2": null,
          "postal_code": "11111",
          "state": "CA"
        },
        "carrier": null,
        "name": "example username",
        "phone": null,
        "tracking_number": null
      },
      "source": null,
      "statement_descriptor": null,
      "statement_descriptor_suffix": null,
      "status": "requires_payment_method",
      "transfer_data": null,
      "transfer_group": null
    }
  },
  "livemode": false,
  "pending_webhooks": 2,
  "request": {
    "id": "req_iopIfwbaJIDNrU",
    "idempotency_key": "95faad4b-7cdc-4271-b9eb-c70eae570a33"
  },
  "type": "payment_intent.created"
}
|]


customerCreated :: LBS.ByteString
customerCreated = [r|
{
  "id": "evt_1LxsEGBHXBAMm9bPNpMsfAwM",
  "object": "event",
  "api_version": "2019-11-05",
  "created": 1666962248,
  "data": {
    "object": {
      "id": "cus_MhGlMSuYwsznIR",
      "object": "customer",
      "address": null,
      "balance": 0,
      "created": 1666962248,
      "currency": null,
      "default_currency": null,
      "default_source": null,
      "delinquent": false,
      "description": "(created by Stripe CLI)",
      "discount": null,
      "email": null,
      "invoice_prefix": "4DEA2542",
      "invoice_settings": {
        "custom_fields": null,
        "default_payment_method": null,
        "footer": null,
        "rendering_options": null
      },
      "livemode": false,
      "metadata": {
      },
      "name": null,
      "next_invoice_sequence": 1,
      "phone": null,
      "preferred_locales": [

      ],
      "shipping": null,
      "sources": {
        "object": "list",
        "data": [

        ],
        "has_more": false,
        "total_count": 0,
        "url": "/v1/customers/cus_MhGlMSuYwsznIR/sources"
      },
      "subscriptions": {
        "object": "list",
        "data": [

        ],
        "has_more": false,
        "total_count": 0,
        "url": "/v1/customers/cus_MhGlMSuYwsznIR/subscriptions"
      },
      "tax_exempt": "none",
      "tax_ids": {
        "object": "list",
        "data": [

        ],
        "has_more": false,
        "total_count": 0,
        "url": "/v1/customers/cus_MhGlMSuYwsznIR/tax_ids"
      },
      "tax_info": null,
      "tax_info_verification": null,
      "test_clock": null
    }
  },
  "livemode": false,
  "pending_webhooks": 2,
  "request": {
    "id": "req_E1nCrCScXzp8ua",
    "idempotency_key": "42b72b96-3fde-47a7-bf5d-02779bbbbd5d"
  },
  "type": "customer.created"
}
|]
