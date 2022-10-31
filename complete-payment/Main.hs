{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  ) where

import Text.Printf
  ( printf
  )

import Data.Text
  ( Text
  , pack
  , unpack
  )

import GHC.Natural
 ( naturalFromInteger
 )

import Data.Text.Encoding
  ( encodeUtf8
  )

import Data.ByteString
  ( ByteString
  , readFile
  )
import Data.ByteString.Char8
  ( strip
  )

import Text.RawString.QQ
  ( r
  )

import Options.Applicative
  ( Parser
  , ParserInfo
  , strOption
  , option
  , auto
  , long
  , help
  , showDefault
  , value
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  , header
  , execParser
  )

import Network.HTTP.Client
  ( Request(method, requestBody, requestHeaders, path)
  , RequestBody(RequestBodyBS)
  , parseRequest
  , newManager
  , defaultManagerSettings
  , httpLbs
  , responseStatus
  , responseBody
  )

import Data.Time.Clock.POSIX
  ( getPOSIXTime
  )

import Stripe.Concepts
  ( WebhookSecretKey(WebhookSecretKey)
  )

import PaymentServer.Processors.Stripe
  ( stripeSignature
  )

data Config = Config
  { configServerURL         :: Text
  , configVoucher           :: Text
  , configWebhookSecretPath :: FilePath
  }

config :: Parser Config
config = Config
  <$> strOption
  ( long "server-url"
    <> help "The root URL of the PaymentServer on which to complete the payment."
    <> showDefault
    <> value "http://localhost:8000/"
  )
  <*> strOption
  ( long "voucher"
    <> help "The voucher for which to complete payment."
  )
  <*> strOption
  ( long "webhook-secret-path"
  <> help "The path to a file containing the webhook secret to use to sign the request."
  )

options :: ParserInfo Config
options = info (config <**> helper)
  ( fullDesc
  <> progDesc ""
  <> header ""
  )

-- Construct the request body for a `checkout.session.complete` event
-- containing the given voucher.
completePaymentBody :: Text -> ByteString
completePaymentBody =
  encodeUtf8 . pack . printf template
  where
    template = [r|
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
      "client_reference_id": "%s",
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


main :: IO ()
main = do
  Config
    { configServerURL
    , configVoucher
    , configWebhookSecretPath
    } <- execParser options

  let body = completePaymentBody configVoucher
  webhookSecret <- WebhookSecretKey . strip <$> Data.ByteString.readFile configWebhookSecretPath
  now <- naturalFromInteger . truncate <$> getPOSIXTime

  req <- parseRequest . unpack $ configServerURL
  let req' = req
        { method = "POST"
        , path = "/v1/stripe/webhook"
        , requestBody = RequestBodyBS body
        , requestHeaders =
          [ ( "Stripe-Signature"
            , stripeSignature webhookSecret now body
            )
          , ( "Content-Type"
            , "application/json; charset=utf-8"
            )
          ]
        }

  manager <- newManager defaultManagerSettings
  response <- httpLbs req' manager
  print ((responseStatus response), (responseBody response))
