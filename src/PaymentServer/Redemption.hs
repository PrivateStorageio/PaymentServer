{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module implements the exposed interface for redeeming a voucher for
-- signatures.
module PaymentServer.Redemption
  ( RedemptionAPI
  , Redeem(Redeem)
  , RedemptionConfig
    ( RedemptionConfig
    , redemptionConfigMaxCounter
    , redemptionConfigTokensPerVoucher
    , redemptionConfigIssue
    )
  , redemptionServer
  ) where

import Prelude hiding
  ( concat
  )

import GHC.Generics
  ( Generic
  )

import Control.Retry
  ( retrying
  , constantDelay
  , limitRetries
  )
import Control.Monad
  ( when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , pack
  , concat
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Data.Aeson
  ( ToJSON(toJSON, toEncoding)
  , FromJSON(parseJSON)
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
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
  , err500
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
import qualified Prometheus as P
import PaymentServer.Persistence
  ( VoucherDatabase(redeemVoucherWithCounter)
  , RedeemError(NotPaid, AlreadyRedeemed, DuplicateFingerprint, DatabaseUnavailable)
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

data RedemptionConfig
  = RedemptionConfig
  { redemptionConfigMaxCounter :: Int
  , redemptionConfigTokensPerVoucher :: Int
  , redemptionConfigIssue :: Issuer
  }

data Result
  = Unpaid -- ^ A voucher has not been paid for.
  | DoubleSpend -- ^ A voucher has already been redeemed.
  | OtherFailure Text -- ^ Some other unrecognized failure mode.
  -- | Given counter was not in the expected range
  | CounterOutOfBounds Int Int Int
  | Succeeded PublicKey [Signature] Proof
  deriving (Show, Eq)

-- | A complete redemption attempt which can be presented at the redemption
-- endpoint.
data Redeem
  = Redeem
  { redeemVoucher :: Voucher        -- ^ The voucher being redeemed.
  , redeemTokens :: [BlindedToken]  -- ^ Tokens to be signed as part of this redemption.
  , redeemCounter :: Int            -- ^ Counter tag on this redemption.
  } deriving (Show, Eq, Generic)

instance FromJSON Redeem where
  parseJSON = withObject "redeem" $ \o -> do
    voucher <- o .: "redeemVoucher"
    tokens <- o .: "redeemTokens"
    counter <- o .:? "redeemCounter" .!= 0
    return $ Redeem voucher tokens counter

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
  toJSON (CounterOutOfBounds min max received) = object
    [ "success" .= False
    , "reason" .= ("counter-out-of-bounds" :: Text)
    , "min" .= min
    , "max" .= max
    , "received" .= received
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

jsonErr err reason = err
  { errBody = encode reason
  , errHeaders = [ ("Content-Type", "application/json;charset=utf-8") ]
  }

redemptionServer :: VoucherDatabase d => RedemptionConfig -> d -> Server RedemptionAPI
redemptionServer = redeem

-- | Try an operation repeatedly for several minutes with a brief delay
-- between tries.
retry :: IO (Either RedeemError a) -> IO (Either RedeemError a)
retry op =
  retrying policy shouldRetry $ \_ -> op
  where
    -- Total duration for which to retry in milliseconds.
    totalRetryDuration = 10 * 60 * 1000
    -- Time to delay between each try in milliseconds.
    perRetryDelay = 500
    -- Limit on the number of retries.
    numRetries = totalRetryDuration `div` perRetryDelay

    policy = constantDelay (perRetryDelay * 1000) <> limitRetries numRetries
    shouldRetry status value =
      case value of
        Left NotPaid -> return True
        _ -> return False


-- | Compute the number of tokens which should be present in a certain
-- redemption group.
tokenCountForGroup
  :: Int
  -- ^ The total number of redemption groups.
  -> Int
  -- ^ The total number of tokens across all redemption groups.
  -> Int
  -- ^ The number of the redemption group for which to compute the token
  -- count.
  -> Maybe Int
  -- ^ Nothing if the parameters are incoherent.  Otherwise, Just the number
  -- of tokens expected in the specified group.
tokenCountForGroup numGroups totalTokens groupNumber
  | totalTokens < numGroups = Nothing
  | groupNumber >= numGroups || groupNumber < 0 = Nothing
  | otherwise = Just $ groupSize + groupSizeAdjustment
  where
    (groupSize, remainder) = totalTokens `divMod` numGroups
    groupSizeAdjustment = if groupNumber < remainder then 1 else 0


-- | Handler for redemption requests.  Use the database to try to redeem the
-- voucher and return signatures.  Return a failure if this is not possible
-- (eg because the voucher was already redeemed).
redeem :: VoucherDatabase d => RedemptionConfig -> d -> Redeem -> Handler Result
redeem (RedemptionConfig numGroups tokensPerVoucher issue) database (Redeem voucher tokens counter) =
  if counter < 0 || counter >= numGroups then
    throwError $ jsonErr err400 (CounterOutOfBounds 0 numGroups counter)
  else
    case tokenCountForGroup numGroups tokensPerVoucher (fromIntegral counter) of
      Nothing ->
        throwError $ jsonErr err500 $ OtherFailure "invalid redemption counter"
      Just allowedTokenCount ->
        if allowedTokenCount /= length tokens
        then throwError $ jsonErr err400 $ OtherFailure "wrong number of tokens"
        else redeem
  where
    fingerprint = fingerprintFromTokens tokens

    redeemOnce :: IO (Either RedeemError Bool)
    redeemOnce =
      redeemVoucherWithCounter database voucher fingerprint (fromIntegral counter)

    redeem :: Handler Result
    redeem = do
      redeemResult <- liftIO $ retry redeemOnce
      case redeemResult of
        Left NotPaid -> do
          throwError $ jsonErr err400 Unpaid
        Left AlreadyRedeemed -> do
          throwError $ jsonErr err400 DoubleSpend
        Left DuplicateFingerprint -> do
          throwError $ jsonErr err400 $ OtherFailure "fingerprint already used"
        Left DatabaseUnavailable -> do
          throwError $ jsonErr err500 $ OtherFailure "database temporarily unavailable"
        Right fresh ->
          case issue tokens of
            Left reason -> do
              throwError $ jsonErr err400 $ OtherFailure reason
            Right (ChallengeBypass key signatures proof) -> do
              let count = fromInteger . toInteger . length $ signatures
              -- addCounter returns bool indicating whether its argument was
              -- greater than or equal to zero or not.  We don't care.  length
              -- is greater than or equal to zero.
              liftIO . when fresh $ (P.addCounter signaturesIssued count >>= \_ -> return ())
              return $ Succeeded key signatures proof


metricName :: Text -> Text
metricName name = mappend "redemption_" name


signaturesIssued :: P.Counter
signaturesIssued
  = P.unsafeRegister
  $ P.counter
  $ P.Info (metricName "signatures_issued")
  "The number of unique signatures which have been issued to clients."


-- | Compute a cryptographic hash (fingerprint) of a list of tokens which can
-- be used as an identifier for this exact sequence of tokens.
fingerprintFromTokens :: [BlindedToken] -> Fingerprint
fingerprintFromTokens =
  pack . show . hashWith SHA3_512 . encodeUtf8 . concat
