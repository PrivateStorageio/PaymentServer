{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
  )
import PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  )
import PaymentServer.Persistence
  ( VoucherDatabase
  )

-- | This is the complete type of the server API.
type PaymentServerAPI
  =    "v1" :> "stripe" :> StripeAPI
  -- :<|> "v1" :> "redeem" :> RedeemAPI

-- | Create a server which uses the given database.
paymentServer :: VoucherDatabase d => d -> Server PaymentServerAPI
paymentServer = stripeServer

paymentServerAPI :: Proxy PaymentServerAPI
paymentServerAPI = Proxy

-- | Create a Servant Application which serves the payment server API using
-- the given database.
paymentServerApp :: VoucherDatabase d => d -> Application
paymentServerApp = (serve paymentServerAPI) . paymentServer
