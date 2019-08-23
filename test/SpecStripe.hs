{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Test suite for Stripe support in the payment server.
--

module SpecStripe where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
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
  ( Property
  , Gen
  , arbitrary
  , suchThat
  , suchThatMap
  , property
  , generate
  , forAll
  , (===)
  , (=/=)
  )
import Test.QuickCheck.Instances.Tuple
  ( (>*<)
  )
import Util.WAI
  ( postJSON
  )
import Util.Gen
  ( chargeSucceededEvents
  , hasVoucher
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
import Web.Stripe.Types
  ( MetaData(MetaData)
  )
import PaymentServer.Processors.Stripe
  ( StripeAPI
  , stripeServer
  , getVoucher
  )
import PaymentServer.Persistence
  ( Voucher
  , memory
  )
import Data.List.Index
  ( insertAt
  )

stripeAPI :: Proxy StripeAPI
stripeAPI = Proxy

app :: IO Application
app = memory >>= return . stripeServer >>= return . serve stripeAPI

aChargeEvent :: IO LazyBS.ByteString
aChargeEvent = encode <$> generate chargeSucceededEvents

spec_webhook :: Spec
spec_webhook = with app $ do
  -- I would like to make these property tests but I can't figure out how to
  -- use QuickCheck (or Hedgehog) to write property tests for web code.

  describe "error behavior of POST /webhook" $ do
    it "responds to non-JSON Content-Type with 415 (Unsupported Media Type)" $
      post "/webhook" "xxx" `shouldRespondWith` 415

    it "responds to JSON non-Event body with 400 (Invalid Request)" $
      postJSON "/webhook" "{}" `shouldRespondWith` 400

  describe "success behavior of POST /webhook" $
    it "responds to a JSON Event body with 200 (OK)" $ do
      event <- liftIO aChargeEvent
      postJSON "/webhook" event `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
bodyMatcher _ "{}" = Nothing
bodyMatcher _ body = Just $ show body

metaDatasWithoutVoucher = (arbitrary :: Gen MetaData) `suchThat` (not . hasVoucher)

-- Just filtering out random metadatas that don't have a voucher makes for an
-- incredibly inefficient generator.  So start without a voucher and then add
-- one.
metaDatasWithVoucher = ((arbitrary :: Gen Voucher) >*< metaDatasWithoutVoucher) `suchThatMap` (Just. uncurry addVoucher)

addVoucher :: Voucher -> MetaData -> MetaData
addVoucher voucher (MetaData []) = MetaData [("Voucher", voucher)]
addVoucher voucher (MetaData items) =
  MetaData (insertAt (1234567 `mod` length items) ("Voucher", voucher) items)

prop_getVoucherFindsVoucher :: Property
prop_getVoucherFindsVoucher = forAll metaDatasWithVoucher $ \x ->
  getVoucher x =/= Nothing

prop_getVoucherWithoutVoucher :: Property
prop_getVoucherWithoutVoucher = forAll metaDatasWithoutVoucher $ \x ->
  getVoucher x === Nothing
