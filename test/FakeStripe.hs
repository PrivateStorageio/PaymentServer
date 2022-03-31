{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FakeStripe
  ( ChargeId(ChargeId)
  , withFakeStripe
  , chargeOkay
  , chargeFailed
  , cardError
  , apiError
  ) where

import Text.RawString.QQ

import Data.ByteString.Lazy
   ( ByteString
   )

import Data.Time.Clock
  ( UTCTime(UTCTime)
  , secondsToDiffTime
  )

import Data.Time.Calendar
  ( Day(ModifiedJulianDay)
  )

import Network.HTTP.Types
  ( status200
  , status400
  )

import Network.Wai
  ( Application
  , responseLBS
  )

import Network.Wai.Handler.Warp
  ( testWithApplication
  )

import Web.Stripe.Client
  ( StripeConfig(StripeConfig)
  , StripeKey(StripeKey)
  , Protocol(HTTP)
  , Endpoint(Endpoint)
  )

import Web.Stripe.Types
  ( ChargeId(ChargeId)
  )

cardError :: ByteString
cardError = [r|
{
  "error": {
    "type": "card_error",
    "code": "expired_card",
    "message": "Your card is expired."
  }
}
|]

apiError :: ByteString
apiError = [r|
{
  "error": {
    "type": "api_error",
    "code": "api_key_expired",
    "message": "The API key provided has expired."
  }
}
|]

aCharge :: ByteString
aCharge = [r|
{
  "id": "ch_1Fwa4NBHXBAMm9bPFRPo6UBt",
  "object": "charge",
  "amount": 100,
  "amount_captured": 0,
  "amount_refunded": 0,
  "application": null,
  "application_fee": null,
  "application_fee_amount": null,
  "balance_transaction": "txn_1FrU7mBHXBAMm9bP9WvFFzRG",
  "billing_details": {
    "address": {
      "city": "",
      "country": "US",
      "line1": "",
      "line2": "",
      "postal_code": "",
      "state": "Select State"
    },
    "email": null,
    "name": "",
    "phone": null
  },
  "calculated_statement_descriptor": null,
  "captured": false,
  "created": 1577996099,
  "currency": "usd",
  "customer": null,
  "description": null,
  "disputed": false,
  "failure_code": "card_declined",
  "failure_message": "Your card was declined.",
  "fraud_details": {
    "stripe_report": "fraudulent"
  },
  "invoice": null,
  "livemode": false,
  "metadata": {
    "Voucher": "EQwTqWXIjKF5MWMZmDYKJvbtiaVBlDecmk__bytPlK_l"
  },
  "on_behalf_of": null,
  "order": null,
  "outcome": {
    "network_status": "not_sent_to_network",
    "reason": "merchant_blacklist",
    "risk_level": "highest",
    "risk_score": 90,
    "seller_message": "Stripe blocked this payment.",
    "type": "blocked"
  },
  "paid": false,
  "payment_intent": null,
  "payment_method": "card_1Fwa4MBHXBAMm9bPyw1fke3O",
  "payment_method_details": {
    "card": {
      "brand": "visa",
      "checks": {
        "address_line1_check": null,
        "address_postal_code_check": null,
        "cvc_check": "unavailable"
      },
      "country": "US",
      "exp_month": 11,
      "exp_year": 2022,
      "fingerprint": "5COYv5EoHE9ZE82J",
      "funding": "credit",
      "installments": null,
      "last4": "0019",
      "network": "visa",
      "three_d_secure": null,
      "wallet": null
    },
    "type": "card"
  },
  "receipt_email": null,
  "receipt_number": null,
  "receipt_url": null,
  "refunded": false,
  "refunds": {
    "object": "list",
    "data": [],
    "has_more": false,
    "url": "/v1/charges/ch_1Fwa4NBHXBAMm9bPFRPo6UBt/refunds"
  },
  "review": null,
  "shipping": null,
  "source_transfer": null,
  "statement_descriptor": null,
  "statement_descriptor_suffix": null,
  "status": "failed",
  "transfer_data": null,
  "transfer_group": null
}
|]



-- Accept a charge creation and respond in the affirmative.
chargeOkay :: Application
chargeOkay req respond =
  respond . responseLBS status200 [] $ aCharge

chargeFailed :: ByteString -> Application
chargeFailed stripeResponse req respond =
  respond . responseLBS status400 [] $ stripeResponse

-- Pass a Stripe-flavored configuration for a running Wai application to a
-- function and evaluate the resulting IO action.
withFakeStripe :: IO Application -> (StripeConfig -> IO a) -> IO a
withFakeStripe app f =
  testWithApplication app $ f . makeConfig
  where
    makeConfig = StripeConfig stripeKey . Just . Endpoint "127.0.0.1" HTTP
    stripeKey = StripeKey "pk_test_aaaaaaaaaaaaaaaaaaaaaa"
