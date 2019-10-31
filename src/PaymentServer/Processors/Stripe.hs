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
import Control.Monad
  ( mzero
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  , unpack
  )
import Text.Printf
  ( printf
  )
import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , Value(Object)
  , object
  , (.:)
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
  , (:<|>)(..)
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
import Web.Stripe.Charge
  ( createCharge
  , Amount(..)
  )
import Web.Stripe.Client
  ( StripeConfig(..)
  , StripeKey(..)
  )
import Web.Stripe
  ( stripe
  )

data Acknowledgement = Ok

instance ToJSON Acknowledgement where
  toJSON Ok = object []

type StripeAPI = WebhookAPI
               :<|> ChargesAPI

type WebhookAPI = "webhook" :> ReqBody '[JSON] Event :> Post '[JSON] Acknowledgement

-- | getVoucher finds the metadata item with the key `"Voucher"` and returns
-- the corresponding value, or Nothing.
getVoucher :: MetaData -> Maybe Voucher
getVoucher (MetaData []) = Nothing
getVoucher (MetaData (("Voucher", value):xs)) = Just value
getVoucher (MetaData (x:xs)) = getVoucher (MetaData xs)

stripeServer :: VoucherDatabase d => d -> ByteString -> Server StripeAPI
stripeServer d key = (webhook d)
                     :<|> (charge d key)

-- | Process charge succeeded events
webhook :: VoucherDatabase d => d -> Event -> Handler Acknowledgement
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


-- | Browser facing API that takes token, voucher and a few other information
-- and calls stripe charges API. If payment succeeds, then the voucher is stored
-- in the voucher database.
type ChargesAPI = "charge" :> ReqBody '[JSON] Charges :> Post '[JSON] Acknowledgement

data Charges = Charges
  { token :: Text
  , voucher :: Voucher
  , amount :: Int
  , currency :: Text
  } deriving (Show, Eq)

instance FromJSON Charges where
  parseJSON (Object v) = Charges <$>
                         v .: "token" <*>
                         v .: "voucher" <*>
                         v .: "amount" <*>
                         v .: "currency"
  parseJSON _ = mzero

charge :: VoucherDatabase d => d -> ByteString -> Charges -> Handler Acknowledgement
charge d key (Charges token voucher amount currency) = do
  -- call the stripe Charge API (with token, voucher in metadata, amount, currency etc
  -- and if the Charge is okay, then return set the voucher as "paid" in the database.
  let config = StripeConfig (StripeKey key) Nothing
  result <- liftIO $ stripe config $ createCharge (Amount amount) (read (unpack currency))
  return Ok
