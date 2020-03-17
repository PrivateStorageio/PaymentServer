{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module implements the exposed interface for redeeming a voucher for
-- signatures.
module PaymentServer.Redemption
  ( RedemptionAPI
  , Redeem(Redeem)
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
  , RedeemError(NotPaid, AlreadyRedeemed)
  , Fingerprint
  , Voucher
  )
import PaymentServer.Issuer
  ( Signature
  , PublicKey
  , Proof
  , BlindedToken
  , ChallengeBypass(ChallengeBypass)
  , Issuer
  )

data Result
  = Unpaid -- ^ A voucher has not been paid for.
  | DoubleSpend -- ^ A voucher has already been redeemed.
  | OtherFailure Text -- ^ Some other unrecognized failure mode.
  | Succeeded PublicKey [Signature] Proof
  deriving (Show, Eq)

-- | A complete redemption attempt which can be presented at the redemption
-- endpoint.
data Redeem
  = Redeem
  { redeemVoucher :: Voucher        -- ^ The voucher being redeemed.
  , redeemTokens :: [BlindedToken]  -- ^ Tokens to be signed as part of this redemption.
  } deriving (Show, Eq, Generic)

instance FromJSON Redeem

instance ToJSON Redeem where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Result where
  toJSON Unpaid = object
    [ "success" .= False
    , "reason" .= ("unpaid" :: Text)
    ]
  toJSON DoubleSpend = object
    [ "success" .= False
    , "reason" .= ("double-spend" :: Text)
    ]
  toJSON (OtherFailure description) = object
    [ "success" .= False
    , "reason" .= description
    ]
  toJSON (Succeeded key signatures proof) = object
    [ "success" .= True
    , "public-key" .= key
    , "signatures" .= signatures
    , "proof" .= proof
    ]

instance FromJSON Result where
    parseJSON = withObject "Result" $ \v ->
      v .: "success" >>= \success ->
      if success then
        Succeeded
        <$> v .: "public-key"
        <*> v .: "signatures"
        <*> v .: "proof"
      else do
        reason <- v .: "reason"
        if reason == "unpaid"
        then return Unpaid
        else if reason == "double-spend"
        then return DoubleSpend
        else return $ OtherFailure reason

type RedemptionAPI = ReqBody '[JSON] Redeem :> Post '[JSON] Result

jsonErr400 reason = err400
  { errBody = encode reason
  , errHeaders = [ ("Content-Type", "application/json;charset=utf-8") ]
  }

redemptionServer :: VoucherDatabase d => Issuer -> d -> Server RedemptionAPI
redemptionServer = redeem

-- | Handler for redemption requests.  Use the database to try to redeem the
-- voucher and return signatures.  Return a failure if this is not possible
-- (eg because the voucher was already redeemed).
redeem :: VoucherDatabase d => Issuer -> d -> Redeem -> Handler Result
redeem issue database (Redeem voucher tokens) = do
  let fingerprint = fingerprintFromTokens tokens
  result <- liftIO $ PaymentServer.Persistence.redeemVoucher database voucher fingerprint
  case result of
    Left NotPaid -> do
      throwError $ jsonErr400 Unpaid
    Left AlreadyRedeemed -> do
      throwError $ jsonErr400 DoubleSpend
    Right () -> do
      let result = issue tokens
      case result of
        Left reason -> do
          throwError $ jsonErr400 $ OtherFailure reason
        Right (ChallengeBypass key signatures proof) ->
          return $ Succeeded key signatures proof

-- | Compute a cryptographic hash (fingerprint) of a list of tokens which can
-- be used as an identifier for this exact sequence of tokens.
fingerprintFromTokens :: [BlindedToken] -> Fingerprint
fingerprintFromTokens =
  let
    hash = pack . show . hashWith SHA3_512 . encodeUtf8
  in
    foldl (\b a -> hash $ a `mappend` b) "" . map hash
