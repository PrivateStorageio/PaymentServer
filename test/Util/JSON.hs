{-# LANGUAGE OverloadedStrings #-}

module Util.JSON where

import Web.Stripe.Types
  ( Charge(Charge)
  , ChargeId(ChargeId)
  , InvoiceId(InvoiceId)
  , Amount(Amount)
  , Currency(USD)
  )
import Web.Stripe.Event
  ( Event(Event)
  , EventData(ChargeEvent)
  , EventType(ChargeSucceededEvent)
  , EventId(EventId)
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , Value(String, Number)
  , object
  , (.=)
  )

instance ToJSON Event where
  toJSON (Event
           eventId
           eventCreated
           eventLiveMode
           eventType
           eventData
           eventObject
           eventPendingWebHooks
           eventRequest)
    = object
    [ "id" .= eventId
    , "object" .= eventObject
    , "api_version" .= String "2018-05-21"
    , "created" .= eventCreated
    , "data" .= object [ "object" .= eventData ]
    , "type" .= eventType
    , "livemode" .= eventLiveMode
    , "pending_webhooks" .= eventPendingWebHooks
    , "request" .= eventRequest
    ]

instance ToJSON EventId where
  toJSON (EventId eId) = String eId

instance ToJSON InvoiceId where
  toJSON (InvoiceId iId) = String iId

instance ToJSON ChargeId where
  toJSON (ChargeId cId) = String cId

instance ToJSON Currency where
  toJSON USD = "USD"

instance ToJSON EventType where
  toJSON ChargeSucceededEvent = "charge.succeeded"

instance ToJSON EventData where
  toJSON (ChargeEvent charge) = toJSON charge

instance ToJSON Amount where
  toJSON (Amount a) = Number $ fromIntegral a

instance ToJSON Charge where
  toJSON (Charge
           chargeId
           chargeObject
           chargeCreated
           chargeLiveMode
           chargePaid
           chargeAmount
           chargeCurrency
           chargeRefunded
           chargeCreditChard
           chargeCaptured
           chargeRefunds
           chargeBalanceTransaction
           chargeFailureMessage
           chargeFailureCode
           chargeAmountRefunded
           chargeCustomerId
           chargeInvoice
           chargeDescription
           chargeDispute
           chargeMetaData
           chargeStatementDescription
           chargeReceiptEmail
           chargeNumber
         )
    = object
    [ "id"         .= chargeId
    , "object"     .= chargeObject
    , "amount"     .= chargeAmount
    -- , "amount_refunded" .= chargeAmountRefunded
    -- , "balance_transaction" .= chargeBalanceTransaction
    , "captured" .= chargeCaptured
    , "created"    .= chargeCreated
    , "currency"   .= chargeCurrency
    -- , "customer" .= chargeCustomerId
    -- , "description" .= chargeDescription
    -- , "dispute" .= chargeDispute
    -- , "failure_code" .= chargeFailureCode
    -- , "failure_message" .= chargeFailureMessage
    -- , "invoice" .= chargeInvoice
    , "livemode" .= chargeLiveMode
    ]
