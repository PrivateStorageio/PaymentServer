{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | A module which provides Prometheus metrics publishing.  Individual
-- metrics are defined elsewhere in the codebase but they'll all be published
-- by this module.

module PaymentServer.Metrics
  ( MetricsAPI
  , metricsAPI
  , metricsServer
  ) where

import Data.Text.Lazy
  ( Text
  )
import Data.Text.Lazy.Encoding
  ( decodeUtf8
  )

import Prometheus
  ( exportMetricsAsText
  )

import Servant
  ( Proxy(Proxy)
  , Server
  , Handler
  , Get
  , PlainText
  , (:>)
  )

type MetricsAPI = "metrics" :> Get '[PlainText] Text

metricsAPI :: Proxy MetricsAPI
metricsAPI = Proxy

metricsServer :: Server MetricsAPI
metricsServer = metrics

metrics :: Handler Text
metrics = exportMetricsAsText >>= return . decodeUtf8
