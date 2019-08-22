{-# LANGUAGE OverloadedStrings #-}

--
-- Test suite for Stripe support in the payment server.
--

module SpecStripe where

import Data.ByteString as BS
import Data.ByteString.Lazy as LazyBS
import Data.Aeson
  ( encode
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.Hspec.Wai
  ( WaiSession
  , WaiExpectation
  , MatchBody(MatchBody)
  , ResponseMatcher(matchBody)
  , Body
  , with
  , post
  , shouldRespondWith
  , liftIO
  )
import Test.QuickCheck
  ( generate
  )
import Util.WAI
  ( postJSON
  )
import Util.Gen
  ( chargeSucceededEvents
  )
import Util.JSON
  ( -- ToJSON instance for Event
  )
import Network.HTTP.Types
  ( Header
  )
import Servant
  ( Application
  , Proxy(Proxy)
  , serve
  )
import PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  )

stripeAPI :: Proxy StripeAPI
stripeAPI = Proxy

app :: Application
app = serve stripeAPI stripeServer

aChargeEvent :: IO LazyBS.ByteString
aChargeEvent = encode <$> generate chargeSucceededEvents

spec_webhook :: Spec
spec_webhook = with (return app) $
  -- I would like to make these property tests but I can't figure out how to
  -- use QuickCheck (or Hedgehog) to write property tests for web code.

  describe "error behavior of POST /webhook" $ do
    it "responds to non-JSON Content-Type with 415 (Unsupported Media Type)" $
      post "/webhook" "xxx" `shouldRespondWith` 415

    it "responds to JSON non-Event body with 400 (Invalid Request)" $
      postJSON "/webhook" "{}" `shouldRespondWith` 400

spec_webhook' :: Spec
spec_webhook' = with (return app) $
  describe "success behavior of POST /webhook" $
    it "responds to a JSON Event body with 200 (OK)" $ do
      event <- liftIO aChargeEvent
      postJSON "/webhook" event `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
bodyMatcher _ body = if body == "{}" then Nothing else Just $ show body
