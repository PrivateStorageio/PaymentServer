{-# LANGUAGE OverloadedStrings #-}

-- | Tests related to PaymentServer.Metrics and the metrics it exposes.

module Metrics
  ( tests
  ) where

import Data.ByteString.Lazy.Char8
  ( pack
  )

import Test.Tasty
  ( TestTree
  , testGroup
  )

import Test.Tasty.HUnit
  ( testCase
  , assertEqual
  )

import Network.HTTP.Types
  ( methodGet
  )
import Network.Wai
  ( defaultRequest
  , requestMethod
  )
import Network.Wai.Test
  ( Session
  , SResponse
  , runSession
  , setPath
  , request
  , assertStatus
  , assertContentType
  , assertBodyContains
  )

import Servant
  ( Application
  , serve
  )

import Prometheus
  ( Info(Info)
  , unsafeRegister
  , counter
  , incCounter
  )

import PaymentServer.Metrics
  ( metricsAPI
  , metricsServer
  )
import PaymentServer.Server
  ( paymentServerApp
  )
import PaymentServer.Persistence
  ( VoucherDatabaseState
  )

tests :: TestTree
tests = testGroup "Metrics"
  [ metricsTests
  , serverTests
  ]

readMetrics :: Session SResponse
readMetrics = request $ setPath defaultRequest { requestMethod = methodGet } "/metrics"

-- Register a counter at the top-level because the registry is global and this
-- lets us avoid thinking about collisions or unregistration.  unsafeRegister
-- is (only) safe for defining a top-level symbol.
aCounter = unsafeRegister $ counter (Info "a_counter" "A test counter.")

metricsTests :: TestTree
metricsTests =
  -- | A ``GET /metrics`` request receives a text/plain OK response containing
  -- current Prometheus-formatted metrics information.
  testCase "plaintext metrics response" $
  let
    app :: Application
    app = serve metricsAPI metricsServer
  in
    flip runSession app $ do
      incCounter aCounter
      response <- readMetrics
      assertStatus 200 response
      assertContentType "text/plain" response
      assertBodyContains expectedMetrics response
      where
        expectedMetrics = pack . unlines $
          [ "# HELP a_counter A test counter."
          , "# TYPE a_counter counter"
          , "a_counter 1.0"
          ]

-- | The metrics endpoint is hooked up to the overall application server.
serverTests :: TestTree
serverTests =
  testCase "metrics endpoint" $
  let
    app :: Application
    app = paymentServerApp mempty undefined undefined undefined (undefined :: VoucherDatabaseState)
  in
    flip runSession app $ do
      response <- readMetrics
      assertStatus 200 response
