{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
  ( app
  )

import Data.ByteString.Lazy as LazyBS
import Data.ByteString as BS

import Data.Text
  ( Text
  )

import Data.Time.Clock
  ( UTCTime
  )

import Data.Aeson
  ( encode
  )

import Web.Stripe.Types
  ( Charge(Charge)
  , StatementDescription(StatementDescription)
  , MetaData(MetaData)
  , Description(Description)
  , CustomerId(CustomerId)
  , InvoiceId(InvoiceId)
  , ChargeId(ChargeId)
  , Expandable(Id)
  , Currency(UnknownCurrency)
  , Amount(Amount)
  , StripeList(StripeList, list, totalCount, hasMore)
  )

import Web.Stripe.Event
  ( Event(Event, eventId, eventCreated, eventLiveMode, eventType, eventData, eventObject, eventPendingWebHooks, eventRequest)
  , EventType(ChargeSucceededEvent)
  , EventData(ChargeEvent)
  , EventId(EventId)
  )

import Test.Hspec
  ( Spec
  , hspec
  , describe
  , it
  )
import Test.Hspec.Wai
  ( WaiSession
  , with
  , post
  , request
  , shouldRespondWith
  )
import Network.Wai.Test
  ( SResponse
  )

import Network.HTTP.Types.Method
  ( methodPost
  )

import Test.QuickCheck
  ( Gen
  , Arbitrary
  , forAll
  , property
  , arbitrary
  )

import Test.QuickCheck.Instances.Time
  ( -- Get the `Gen UTCTime` instance
  )
import Test.QuickCheck.Instances.Text
  ( -- Get the `Gen Text` instance
  )

main :: IO ()
main = hspec spec

-- Post some JSON to a path.
-- Return a function from path to a response
postJSON :: BS.ByteString -> (LazyBS.ByteString -> WaiSession SResponse)
postJSON path =
  request methodPost path [("Content-Type", "application/json")]

instance Arbitrary Charge where
  arbitrary = Charge
    <$> arbitrary         --   chargeId :: ChargeId
    <*> arbitrary         --   chargeObject :: Text
    <*> arbitrary         --   chargeCreated :: UTCTime
    <*> arbitrary         --   chargeLiveMode :: Bool
    <*> arbitrary         --   chargePaid :: Bool
    <*> arbitrary         --   chargeAmount :: Amount
    <*> (return UnknownCurrency) --   chargeCurrency :: Currency
    <*> (return False)    --   chargeRefunded :: Bool
    <*> (return Nothing)  --   chargeCreditCard :: Maybe Card
    <*> arbitrary         --   chargeCaptured :: Bool
    <*> (return mempty)   --   chargeRefunds :: StripeList Refund
    <*> (return Nothing)  --   chargeBalanceTransaction :: Maybe (Expandable TransactionId)
    <*> (return Nothing)  --   chargeFailureMessage :: Maybe Text
    <*> (return Nothing)  --   chargeFailureCode :: Maybe Text
    <*> (return 0)        --   chargeAmountRefunded :: Int
    <*> arbitrary         --   chargeCustomerId :: Maybe (Expandable CustomerId)
    <*> (return Nothing)  --   chargeInvoice :: Maybe (Expandable InvoiceId)
    <*> arbitrary         --   chargeDescription :: Maybe Description
    <*> (return Nothing)  --   chargeDispute :: Maybe Dispute
    <*> arbitrary         --   chargeMetaData :: MetaData
    <*> arbitrary         --   chargeStatementDescription :: Maybe StatementDescription
    <*> arbitrary         --   chargeReceiptEmail :: Maybe Text
    <*> arbitrary         --   chargeReceiptNumber :: Maybe Text


instance Semigroup (StripeList a) where
  -- Very weak implementation
  x <> y = StripeList (list x <> list y) "" "" Nothing (hasMore x || hasMore y)

instance Monoid (StripeList a) where
  mempty = StripeList [] "" "" (Just 0) False

instance Arbitrary Amount where
  arbitrary = Amount <$> arbitrary


instance Arbitrary EventId where
  arbitrary = EventId <$> arbitrary


instance Arbitrary StatementDescription where
  arbitrary = StatementDescription <$> arbitrary


instance Arbitrary MetaData where
  arbitrary = MetaData <$> arbitrary


instance Arbitrary Description where
  arbitrary = Description <$> arbitrary


instance Arbitrary InvoiceId where
  arbitrary = InvoiceId <$> arbitrary


instance Arbitrary ChargeId where
  arbitrary = ChargeId <$> arbitrary


instance Arbitrary CustomerId where
  arbitrary = CustomerId <$> arbitrary


instance Arbitrary a => Arbitrary (Expandable a) where
  arbitrary = Id <$> arbitrary


chargeSucceededEvents :: Gen Event
chargeSucceededEvents =
  Event
  <$> arbitrary -- eventId
  <*> arbitrary -- eventCreated
  <*> arbitrary -- eventLiveMode
  <*> (return ChargeSucceededEvent) -- eventType
  <*> (ChargeEvent
       <$> arbitrary -- the charge
      ) -- eventData
  <*> arbitrary -- eventObject
  <*> arbitrary -- eventPendingWebHooks
  <*> arbitrary -- eventRequest

spec :: Spec
spec = with (return app) $ do
  describe "error behavior of POST /webhook" $ do
    it "responds to non-JSON Content-Type with 400 (Invalid Request)" $
      post "/webhook" "{}" `shouldRespondWith` 400

    it "responds to JSON non-Event body with 400 (Invalid Request)" $
      postJSON "/webhook" "{}" `shouldRespondWith` 400

  describe "success behavior of POST /webhook" $ do
    it "responds to JSON-encoded Event body with 200 (OK)" $
      forAll chargeSucceededEvents $ \event ->
        postJSON "/webhook" (encode event) `shouldRespondWith` 200
