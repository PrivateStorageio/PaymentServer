{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Main
  ( main
  ) where

import Servant
  ( Proxy(Proxy)
  , Server
  , Application
  , serve
  , (:>)
  )
import Network.Wai.Handler.Warp
  ( run
  )
import PaymentServer.Persistence
  ( VoucherDatabase
  , memory
  )
import PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  )

type PaymentServerAPI = "v1" :> "stripe" :> StripeAPI

paymentServer :: VoucherDatabase d => d -> Server PaymentServerAPI
paymentServer = stripeServer

paymentServerAPI :: Proxy PaymentServerAPI
paymentServerAPI = Proxy

paymentServerApp :: VoucherDatabase d => d -> Application
paymentServerApp = (serve paymentServerAPI) . paymentServer

main :: IO ()
main = memory >>= return . paymentServerApp >>= run 8081
