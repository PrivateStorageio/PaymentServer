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
import Test.Hspec.Wai.QuickCheck
  ( property
  )
import Test.QuickCheck
  ( Property
  , Gen
  , arbitrary
  , suchThat
  , suchThatMap
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
  ( GoodChargeEvent(GoodChargeEvent)
  , chargeSucceededEvents
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

spec_webhook :: Spec
spec_webhook = with app $ do
  describe "error behavior of POST /webhook" $ do
    it "responds to non-JSON Content-Type with 415 (Unsupported Media Type)" $
      post "/webhook" "xxx" `shouldRespondWith` 415

    it "responds to JSON non-Event body with 400 (Invalid Request)" $
      postJSON "/webhook" "{}" `shouldRespondWith` 400

  -- I would like to make most or all of these into property tests.  *This*
  -- test shows how you can do it.  Yay.  The main thing (for me, anyway) to
  -- remember is to use `property` from Test.Hspec.Wai.QuickCheck and not from
  -- `Test.QuickCheck`. :/ Unsure whether I love the apparent Haskell
  -- convention of giving the same name to *similar* functions.
  describe "success behavior of POST /webhook" $
    it "responds to a JSON Event body with 200 (OK)" $
    property $
    \(GoodChargeEvent event) ->
      postJSON "/webhook" (encode event) `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }

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
