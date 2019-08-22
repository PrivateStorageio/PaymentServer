{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
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
  ( Charge(Charge)
  )

data Acknowledgement = Ok

instance ToJSON Acknowledgement where
  toJSON Ok = object []

type StripeAPI = "webhook" :> ReqBody '[JSON] Event :> Post '[JSON] Acknowledgement

stripeServer :: Server StripeAPI
stripeServer = webhook

webhook :: Event -> Handler Acknowledgement

-- Process charge succeeded events
webhook Event{eventId=Just (EventId eventId), eventType=ChargeSucceededEvent, eventData=ChargeEvent{}} = do
  return Ok

-- Disregard anything else - but return success so that Stripe doesn't retry.
webhook _ = return Ok
