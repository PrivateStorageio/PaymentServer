{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  , getVoucher
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Text.Printf
  ( printf
  )
import Data.Aeson
  ( ToJSON(toJSON)
  , object
  )
import Servant
  ( Server
  , Handler
  )
import Servant.API
  ( ReqBody
  , JSON
  , Post
  , (:>)
  )
import Web.Stripe.Event
  ( Event(Event, eventId, eventCreated, eventLiveMode, eventType, eventData, eventObject, eventPendingWebHooks, eventRequest)
  , EventId(EventId)
  , EventType(ChargeSucceededEvent)
  , EventData(ChargeEvent)
  )
import Web.Stripe.Types
  ( Charge(Charge, chargeMetaData)
  , MetaData(MetaData)
  )
import PaymentServer.Persistence
  ( Voucher
  , VoucherDatabase(payForVoucher)
  )


data Acknowledgement = Ok

instance ToJSON Acknowledgement where
  toJSON Ok = object []

type StripeAPI = "webhook" :> ReqBody '[JSON] Event :> Post '[JSON] Acknowledgement

-- | getVoucher finds the metadata item with the key `"Voucher"` and returns
-- the corresponding value, or Nothing.
getVoucher :: MetaData -> Maybe Voucher
getVoucher (MetaData []) = Nothing
getVoucher (MetaData (("Voucher", value):xs)) = Just value
getVoucher (MetaData (x:xs)) = getVoucher (MetaData xs)

stripeServer :: VoucherDatabase d => d -> Server StripeAPI
stripeServer = webhook

webhook :: VoucherDatabase d => d -> Event -> Handler Acknowledgement

-- Process charge succeeded events
webhook d Event{eventId=Just (EventId eventId), eventType=ChargeSucceededEvent, eventData=(ChargeEvent charge)} =
  case getVoucher $ chargeMetaData charge of
    Nothing ->
      -- TODO: Record the eventId somewhere.  In all cases where we don't
      -- associate the value of the charge with something in our system, we
      -- probably need enough information to issue a refund.  We're early
      -- enough in the system here that refunds are possible and not even
      -- particularly difficult.
      return Ok
    Just v  -> do
      -- TODO: What if it is a duplicate payment?  payForVoucher should be
      -- able to indicate error I guess.
      () <- liftIO $ payForVoucher d v
      return Ok

-- Disregard anything else - but return success so that Stripe doesn't retry.
webhook d _ =
  -- TODO: Record the eventId somewhere.
  return Ok
