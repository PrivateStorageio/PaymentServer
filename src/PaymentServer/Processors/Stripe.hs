{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  , getVoucher
  , StripeSecretKey
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
import Text.Read
  ( readMaybe
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
  , err400
  , err500
  , ServerError(errBody)
  , throwError
  )
import Servant.API
  ( ReqBody
  , JSON
  , Post
  , (:>)
  , (:<|>)((:<|>))
  )
import Web.Stripe.Event
  ( Event(Event, eventId, eventType, eventData)
  , EventId(EventId)
  , EventType(ChargeSucceededEvent)
  , EventData(ChargeEvent)
  )
import Web.Stripe.Types
  ( Charge(Charge, chargeMetaData)
  , MetaData(MetaData)
  , Currency
  )
import Web.Stripe.Error
  ( StripeError(StripeError)
  )
import Web.Stripe.Charge
  ( createCharge
  , Amount(Amount)
  , TokenId(TokenId)
  )
import Web.Stripe.Client
  ( StripeConfig(StripeConfig)
  , StripeKey(StripeKey)
  )
import Web.Stripe
  ( stripe
  , (-&-)
  )
import PaymentServer.Persistence
  ( Voucher
  , VoucherDatabase(payForVoucher)
  )

type StripeSecretKey = ByteString

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

stripeServer :: VoucherDatabase d => StripeSecretKey -> d -> Server StripeAPI
stripeServer key d = webhook d
                     :<|> charge d key

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
  { token :: Text          -- ^ The text of a Stripe tokenized payment method.
  , voucher :: Voucher     -- ^ The voucher for which this charge will pay.
  , amount :: Int          -- ^ The amount of the charge in the minimum
                           -- currency unit of the target currency (eg for
                           -- USD, cents).
  , currency :: Text       -- ^ The currency in which the charge will be made.
  } deriving (Show, Eq)

instance FromJSON Charges where
  parseJSON (Object v) = Charges <$>
                         v .: "token" <*>
                         v .: "voucher" <*>
                         v .: "amount" <*>
                         v .: "currency"
  parseJSON _ = mzero

-- | call the stripe Charge API (with token, voucher in metadata, amount, currency etc
-- and if the Charge is okay, then set the voucher as "paid" in the database.
charge :: VoucherDatabase d => d -> StripeSecretKey -> Charges -> Handler Acknowledgement
charge d key (Charges token voucher amount currency) = do
  let config = StripeConfig (StripeKey key) Nothing
      tokenId = TokenId token
  currency' <- getCurrency currency
  result <- liftIO $ stripe config $
    createCharge (Amount amount) currency'
      -&- tokenId
      -&- MetaData [("Voucher", voucher)]
  case result of
    Right Charge { chargeMetaData = metadata } ->
      -- verify that we are getting the same metadata that we sent.
      case metadata of
        MetaData [("Voucher", v)] ->
          if v == voucher
            then
            do
              liftIO $ payForVoucher d voucher
              return Ok
            else
            throwError err500 { errBody = "Voucher code mismatch" }
        _ -> throwError err400 { errBody = "Voucher code not found" }
    Left StripeError {} -> throwError err400 { errBody = "Stripe charge didn't succeed" }
    where
      getCurrency :: Text -> Handler Currency
      getCurrency maybeCurrency = do
        case readMaybe (unpack currency) of
          Just currency' -> return currency'
          Nothing -> throwError err400 { errBody = "Invalid currency specified" }
