{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Main
  ( main
  ) where

import Data.Default
  ( def
  )
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
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(Detailed, CustomOutputFormatWithDetails)
  , outputFormat
  , mkRequestLogger
  )
import Network.Wai.Middleware.RequestLogger.JSON
  ( formatAsJSON
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
main = do
  db <- memory
  let app = paymentServerApp db
  logger <- mkRequestLogger $ def { outputFormat = Detailed True}
  run 8081 $ logger app
