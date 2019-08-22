{-# LANGUAGE OverloadedStrings #-}

module Util.Gen
  ( chargeSucceededEvents
  ) where

import Data.Text
  ( Text
  )

import Data.Time.Clock
  ( UTCTime(UTCTime)
  )

import Data.Time.Clock.POSIX
  ( POSIXTime
  , posixSecondsToUTCTime
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
  , Currency(USD, UnknownCurrency)
  , Amount(Amount)
  , StripeList(StripeList, list, totalCount, hasMore)
  )

import Web.Stripe.Event
  ( Event(Event, eventId, eventCreated, eventLiveMode, eventType, eventData, eventObject, eventPendingWebHooks, eventRequest)
  , EventType(ChargeSucceededEvent)
  , EventData(ChargeEvent)
  , EventId(EventId)
  )


import Test.QuickCheck
  ( Gen
  , Arbitrary
  , arbitrary
  , Positive(Positive)
  , oneof
  , suchThatMap
  )

import Test.QuickCheck.Instances.Time
  ( -- Get the `Gen UTCTime` instance
  )
import Test.QuickCheck.Instances.Text
  ( -- Get the `Gen Text` instance
  )

instance Arbitrary Charge where
  arbitrary = Charge
    <$> arbitrary         --   chargeId :: ChargeId
    <*> return "charge"   --   chargeObject :: Text
    <*> posixTimes        --   chargeCreated :: UTCTime
    <*> arbitrary         --   chargeLiveMode :: Bool
    <*> arbitrary         --   chargePaid :: Bool
    <*> arbitrary         --   chargeAmount :: Amount
    <*> oneof
    [ return UnknownCurrency
    , return USD
    ]                     --   chargeCurrency :: Currency
    <*> return False      --   chargeRefunded :: Bool
    <*> return Nothing    --   chargeCreditCard :: Maybe Card
    <*> arbitrary         --   chargeCaptured :: Bool
    <*> return mempty     --   chargeRefunds :: StripeList Refund
    <*> return Nothing    --   chargeBalanceTransaction :: Maybe (Expandable TransactionId)
    <*> return Nothing    --   chargeFailureMessage :: Maybe Text
    <*> return Nothing    --   chargeFailureCode :: Maybe Text
    <*> return 0          --   chargeAmountRefunded :: Int
    <*> arbitrary         --   chargeCustomerId :: Maybe (Expandable CustomerId)
    <*> return Nothing    --   chargeInvoice :: Maybe (Expandable InvoiceId)
    <*> arbitrary         --   chargeDescription :: Maybe Description
    <*> return Nothing    --   chargeDispute :: Maybe Dispute
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
  <*> posixTimes -- eventCreated
  <*> arbitrary -- eventLiveMode
  <*> return ChargeSucceededEvent -- eventType
  <*> (ChargeEvent
       <$> arbitrary -- the charge
      ) -- eventData
  <*> return "event" -- eventObject
  <*> arbitrary -- eventPendingWebHooks
  <*> arbitrary -- eventRequest

posixTimes :: Gen UTCTime
posixTimes = (arbitrary :: Gen Integer) `suchThatMap` (Just . posixSecondsToUTCTime . fromIntegral . abs)

-- dropFractionalSeconds :: UTCTime -> UTCTime
-- dropFractionalSeconds (UTCTime day dayTime) = UTCTime day (round dayTime)
