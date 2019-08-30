{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module implements the exposed interface for redeeming a voucher for
-- signatures.
module PaymentServer.Redemption
  ( RedemptionAPI
  , redemptionServer
  ) where

import GHC.Generics
  ( Generic
  )

import Data.Text
  ( Text
  )
import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON
  , genericToEncoding
  , defaultOptions
  , encode
  , object
  , (.=)
  )
import Servant
  ( Server
  , ServerError(errBody, errHeaders)
  , err400
  , throwError
  )
import Servant.API
  ( JSON
  , Post
  , ReqBody
  , (:>)
  )
import PaymentServer.Persistence
  ( VoucherDatabase
  , Voucher
  )

data Result
  = Failed
  deriving (Show, Eq)

type BlindedToken = Text

data Redeem
  = Redeem { redeemVoucher :: Voucher, redeemTokens :: [BlindedToken] }
  deriving (Show, Eq, Generic)

instance FromJSON Redeem

instance ToJSON Result where
  toJSON Failed = object [ "success" .= False ]

type RedemptionAPI = ReqBody '[JSON] Redeem :> Post '[JSON] Result

jsonErr400 = err400
  { errBody = encode Failed
  , errHeaders = [ ("Content-Type", "application/json") ]
  }

redemptionServer :: VoucherDatabase d => d -> Server RedemptionAPI
redemptionServer _ = redeem

redeem request = return Failed -- throwError jsonErr400
