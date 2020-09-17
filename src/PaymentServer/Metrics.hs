{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module PaymentServer.Metrics
  ( metricsAPI
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
