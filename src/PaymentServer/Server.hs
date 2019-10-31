{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module exposes a Servant-based Network.Wai server for payment
-- interactions.
module PaymentServer.Server
  ( paymentServerApp
  ) where

import Servant
  ( Proxy(Proxy)
  , Server
  , Application
  , serve
  , (:>)
  , (:<|>)((:<|>))
  )
import PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  )
import PaymentServer.Redemption
  ( RedemptionAPI
  , redemptionServer
  )
import PaymentServer.Issuer
  ( Issuer
  )
import PaymentServer.Persistence
  ( VoucherDatabase
  )

-- | This is the complete type of the server API.
type PaymentServerAPI
  =    "v1" :> "stripe" :> StripeAPI
  :<|> "v1" :> "redeem" :> RedemptionAPI

-- | Create a server which uses the given database.
paymentServer :: VoucherDatabase d => Issuer -> d -> Server PaymentServerAPI
paymentServer issuer database =
  stripeServer database "test"
  :<|> redemptionServer issuer database

paymentServerAPI :: Proxy PaymentServerAPI
paymentServerAPI = Proxy

-- | Create a Servant Application which serves the payment server API using
-- the given database.
paymentServerApp :: VoucherDatabase d => Issuer -> d -> Application
paymentServerApp issuer = serve paymentServerAPI . paymentServer issuer
