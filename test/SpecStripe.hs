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
  , generate
  , forAll
  , (===)
  , (=/=)
  )
import Util.WAI
  ( postJSON
  )
import Util.Gen
  ( ChargeEvents(GoodChargeEvent, BadChargeEvent)
  , chargeSucceededEvents
  , metaDatasWithVoucher
  , metaDatasWithoutVoucher
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
    let
      test e =
        postJSON "/webhook" (encode e) `shouldRespondWith` 200 { matchBody = MatchBody bodyMatcher }
      -- For now these are the same.  Maybe they always will be?  The HTTP
      -- behavior is the same though the backend behavior may differ.  Note
      -- that a "test_" prefix would cause tasty-discover to find this and try
      -- to call it - but it can't since it's not a top-level, let alone
      -- exported.
      xtest_postWithEventBody (GoodChargeEvent e) = test e
      xtest_postWithEventBody (BadChargeEvent e) = test e
    in
      property xtest_postWithEventBody



bodyMatcher :: [Network.HTTP.Types.Header] -> Body -> Maybe String
bodyMatcher _ "{}" = Nothing
bodyMatcher _ body = Just $ show body

prop_getVoucherFindsVoucher :: Property
prop_getVoucherFindsVoucher = forAll metaDatasWithVoucher $ \x ->
  getVoucher x =/= Nothing

prop_getVoucherWithoutVoucher :: Property
prop_getVoucherWithoutVoucher = forAll metaDatasWithoutVoucher $ \x ->
  getVoucher x === Nothing
