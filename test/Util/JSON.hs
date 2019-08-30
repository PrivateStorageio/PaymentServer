{-# LANGUAGE OverloadedStrings #-}

module Util.JSON where

import Data.Time.Clock.POSIX
  ( utcTimeToPOSIXSeconds
  )
import Data.HashMap.Lazy
  ( fromList
  )
import qualified Data.Sequence as Seq
import Web.Stripe.Types
  ( Charge(Charge)
  , ChargeId(ChargeId)
  , InvoiceId(InvoiceId)
  , Amount(Amount)
  , Currency(USD, UnknownCurrency)
  , MetaData(MetaData)
  , StripeList(StripeList)
  , Refund(Refund)
  , RefundId(RefundId)
  , TransactionId(TransactionId)
  , Expandable(Id)
  )
import Web.Stripe.Event
  ( Event(Event)
  , EventData(ChargeEvent)
  , EventType(ChargeSucceededEvent)
  , EventId(EventId)
  )
import Data.Aeson
  ( ToJSON(toJSON)
  , Value(String, Number, Object, Array)
  , object
  , (.=)
  )
import Data.Aeson.Types
  ( listValue
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
    , "created" .= utcTimeToPOSIXSeconds eventCreated
    , "data" .= object [ "object" .= eventData ]
    , "type" .= eventType
    , "livemode" .= eventLiveMode
    , "pending_webhooks" .= eventPendingWebHooks
    , "request" .= eventRequest
    ]

instance ToJSON a => ToJSON (Expandable a) where
  toJSON (Id eId) = toJSON eId

instance ToJSON EventId where
  toJSON (EventId eId) = String eId

instance ToJSON InvoiceId where
  toJSON (InvoiceId iId) = String iId

instance ToJSON ChargeId where
  toJSON (ChargeId cId) = String cId

instance ToJSON RefundId where
  toJSON (RefundId rId) = String rId

instance ToJSON TransactionId where
  toJSON (TransactionId tId) = String tId

instance ToJSON Currency where
  toJSON USD = "USD"
  toJSON UnknownCurrency = "???"

instance ToJSON EventType where
  toJSON ChargeSucceededEvent = "charge.succeeded"

instance ToJSON EventData where
  toJSON (ChargeEvent charge) = toJSON charge

instance ToJSON Amount where
  toJSON (Amount a) = Number $ fromIntegral a

instance ToJSON MetaData where
  toJSON (MetaData items) = (Object . fromList . map (\(k, v) -> (k, String v))) items

instance ToJSON Refund where
  toJSON (Refund
           refundId
           refundAmount
           refundCurrency
           refundCreated
           refundObject
           refundCharge
           refundBalanceTransaction
           refundMetaData
         )
    = object
    [ "id" .= refundId
    , "amount" .= refundAmount
    , "currency" .= refundCurrency
    , "created" .= utcTimeToPOSIXSeconds refundCreated
    , "object" .= refundObject
    , "charge" .= refundCharge
    , "balance_transaction" .= refundBalanceTransaction
    , "metadata" .= refundMetaData
    ]

instance (ToJSON a) => ToJSON (StripeList a) where
  toJSON (StripeList elements stripeUrl obj totalCount hasMore)
    = object
    [ "data" .= toJSON elements
    , "url" .= stripeUrl
    , "object" .= obj
    , "has_more" .= hasMore
    ]

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
    , "amount_refunded" .= chargeAmountRefunded
    -- , "balance_transaction" .= chargeBalanceTransaction
    , "captured" .= chargeCaptured
    , "created"    .= utcTimeToPOSIXSeconds chargeCreated
    , "currency"   .= chargeCurrency
    -- , "customer" .= chargeCustomerId
    -- , "description" .= chargeDescription
    -- , "dispute" .= chargeDispute
    -- , "failure_code" .= chargeFailureCode
    -- , "failure_message" .= chargeFailureMessage
    -- , "invoice" .= chargeInvoice
    , "livemode" .= chargeLiveMode
    , "metadata" .= chargeMetaData
    , "paid" .= chargePaid
    , "receipt_email" .= chargeReceiptEmail
    , "refunded" .= chargeRefunded
    , "refunds" .= chargeRefunds
    ]
