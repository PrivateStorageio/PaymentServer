{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module implements the exposed interface for redeeming a voucher for
-- signatures.
module PaymentServer.Redemption
  ( RedemptionAPI
  , BlindedToken
  , Redeem(Redeem)
  , Result(Failed, Succeeded)
  , redemptionServer
  ) where

import GHC.Generics
  ( Generic
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , pack
  )
import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )
import Data.Aeson
  ( ToJSON(toJSON, toEncoding)
  , FromJSON(parseJSON)
  , withObject
  , (.:)
  , genericToEncoding
  , defaultOptions
  , encode
  , object
  , (.=)
  )
import Servant
  ( Server
  , Handler
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
import Crypto.Hash
  ( SHA3_512(SHA3_512)
  , hashWith
  )
import PaymentServer.Persistence
  ( VoucherDatabase(redeemVoucher)
  , Fingerprint
  , Voucher
  )

data Result
  = Failed
  | Succeeded
  deriving (Show, Eq)

-- | A blinded token is presented along with a voucher to be signed and the
-- signatures returned to the caller.
type BlindedToken = Text

data Redeem
  = Redeem { redeemVoucher :: Voucher, redeemTokens :: [BlindedToken] }
  deriving (Show, Eq, Generic)

instance FromJSON Redeem

instance ToJSON Redeem where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Result where
  toJSON Failed = object [ "success" .= False ]
  toJSON Succeeded = object [ "success" .= True ]

instance FromJSON Result where
    parseJSON = withObject "Result" $ \v ->
      v .: "success" >>= \success ->
      return $ if success then Succeeded else Failed

type RedemptionAPI = ReqBody '[JSON] Redeem :> Post '[JSON] Result

jsonErr400 = err400
  { errBody = encode Failed
  , errHeaders = [ ("Content-Type", "application/json;charset=utf-8") ]
  }

redemptionServer :: VoucherDatabase d => d -> Server RedemptionAPI
redemptionServer = redeem

redeem :: VoucherDatabase d => d -> Redeem -> Handler Result
redeem database (Redeem voucher tokens) = do
  let fingerprint = fingerprintFromTokens tokens
  result <- liftIO $ PaymentServer.Persistence.redeemVoucher database voucher fingerprint
  case result of
    Left err -> throwError jsonErr400
    Right () -> return Succeeded

fingerprintFromTokens :: [BlindedToken] -> Fingerprint
fingerprintFromTokens =
  let
    hash = pack . show . hashWith SHA3_512 . encodeUtf8
  in
    foldl (\b a -> hash $ a `mappend` b) "" . map hash
