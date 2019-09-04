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

-- | A cryptographic signature of a blinded token created using our private
-- key.
type Signature = Text

-- | A public key corresponding to our private key.
type PublicKey = Text

-- | A zero-knowledge proof that signatures were created of the corresponding
-- blinded tokens using the corresponding public key's private key.
type Proof = Text

data Result
  = Failed
  | Succeeded PublicKey [Signature] Proof
  deriving (Show, Eq)

-- | A blinded token is presented along with a voucher to be signed and the
-- signatures returned to the caller.
type BlindedToken = Text

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
  toJSON Failed = object [ "success" .= False ]
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
      else
        return Failed

type RedemptionAPI = ReqBody '[JSON] Redeem :> Post '[JSON] Result

jsonErr400 = err400
  { errBody = encode Failed
  , errHeaders = [ ("Content-Type", "application/json;charset=utf-8") ]
  }

redemptionServer :: VoucherDatabase d => d -> Server RedemptionAPI
redemptionServer = redeem

-- | Handler for redemption requests.  Use the database to try to redeem the
-- voucher and return signatures.  Return a failure if this is not possible
-- (eg because the voucher was already redeemed).
redeem :: VoucherDatabase d => d -> Redeem -> Handler Result
redeem database (Redeem voucher tokens) = do
  let fingerprint = fingerprintFromTokens tokens
  result <- liftIO $ PaymentServer.Persistence.redeemVoucher database voucher fingerprint
  case result of
    Left err -> throwError jsonErr400
    Right () -> return $ Succeeded "" [] ""

-- | Compute a cryptographic hash (fingerprint) of a list of tokens which can
-- be used as an identifier for this exact sequence of tokens.
fingerprintFromTokens :: [BlindedToken] -> Fingerprint
fingerprintFromTokens =
  let
    hash = pack . show . hashWith SHA3_512 . encodeUtf8
  in
    foldl (\b a -> hash $ a `mappend` b) "" . map hash
