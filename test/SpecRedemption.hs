{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite related to voucher redemption.

module SpecRedemption where

import Data.ByteString
  ( ByteString
  )
import Text.Printf
  ( printf
  )
import Data.Aeson
 ( decode
 , encode
 )
import Servant
  ( Application
  , Proxy(Proxy)
  , serve
  )
import Test.Hspec
  ( Spec
  , parallel
  , describe
  , it
  , runIO
  )
import Network.HTTP.Types
  ( Header
  )
import Test.Hspec.Wai
  ( ResponseMatcher(matchBody, matchHeaders)
  , (<:>)
  , WaiExpectation
  , Body
  , MatchBody(MatchBody)
  , with
  , shouldRespondWith
  , liftIO
  )
import Test.Hspec.Wai.QuickCheck
  ( property
  )
import Test.QuickCheck.Instances.Text ()
import Util.Spec
  ( wrongMethodNotAllowed
  , nonJSONUnsupportedMediaType
  , wrongJSONInvalidRequest
  )
import Util.WAI
  ( postJSON
  )
import PaymentServer.Issuer
  ( BlindedToken
  , ChallengeBypass(ChallengeBypass)
  , trivialIssue
  )
import PaymentServer.Redemption
  ( RedemptionAPI
  , Redeem(Redeem)
  , Result(Failed, Succeeded)
  , redemptionServer
  )
import PaymentServer.Persistence
  ( RedeemError(NotPaid)
  , Voucher
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , memory
  )

redemptionAPI :: Proxy RedemptionAPI
redemptionAPI = Proxy

app :: VoucherDatabase d => d -> Application
app = serve redemptionAPI . redemptionServer

path = "/"

propertyRedeem :: ByteString -> Voucher -> [BlindedToken] -> ResponseMatcher -> WaiExpectation
propertyRedeem path voucher tokens matcher =
  postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` matcher

-- | A VoucherDatabaseTestDouble has a VoucherDatabase instance which provides
-- a number of different behaviors which are useful to be able to directly
-- test against.
data VoucherDatabaseTestDouble
  -- | A RefuseRedemption database always refuses redemption with a given error.
  = RefuseRedemption RedeemError
  -- | A PermitRedemption database always permits redemption.
  | PermitRedemption
  deriving (Show)

instance VoucherDatabase VoucherDatabaseTestDouble where
  payForVoucher _ voucher = return ()
  redeemVoucher (RefuseRedemption err) _ _ = return $ Left err
  redeemVoucher PermitRedemption _ _ = return $ Right ()

spec_redemption :: Spec
spec_redemption = parallel $ do
  database <- runIO memory
  with (return . app $ database) $
    do
      describe (printf "error behavior of POST %s" (show path)) $
        do
          wrongMethodNotAllowed "GET" path
          nonJSONUnsupportedMediaType path
          wrongJSONInvalidRequest path "{}"

      -- I would rather write these two as property tests but I don't know
      -- how.
      describe "double redemption" $ do
        it "succeeds with the same tokens" $ do
          let voucher = "abc" :: Voucher
          let tokens = [ "def", "ghi" ] :: [BlindedToken]
          liftIO $ payForVoucher database voucher
          propertyRedeem path voucher tokens 200
          propertyRedeem path voucher tokens 200

        it "fails with different tokens" $ do
          let voucher = "jkl" :: Voucher
          let firstTokens = [ "mno", "pqr" ] :: [BlindedToken]
          let secondTokens = [ "stu", "vwx" ] :: [BlindedToken]
          liftIO $ payForVoucher database voucher
          propertyRedeem path voucher firstTokens 200
          propertyRedeem path voucher secondTokens 400


  describe "redemption" $ do
    with (return . app $ RefuseRedemption NotPaid) $
      it "receives a failure response when the voucher is not paid" $ property $
        \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
          propertyRedeem path voucher tokens 400
          { matchBody = matchJSONBody Failed
          -- major/minor, fine.  charset=utf-8... okay.  but really this is
          -- overspecified by encoding the exact byte sequence.  I'd rather
          -- assert semantic equality.
          , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
          }

    with (return $ app PermitRedemption) $
      it "receive a success response when redemption succeeds" $ property $
        \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
          let
            (ChallengeBypass key signatures proof) = trivialIssue tokens
          in
            propertyRedeem path voucher tokens 200
            -- TODO: Get some real crypto involved to be able to replace these
            -- dummy values.
            { matchBody = matchJSONBody $ Succeeded key signatures proof
            , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
            }

    -- it "receive 200 (OK) when the voucher is paid and previously redeemed with the same tokens" $
    --   property $ \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
    --   do
    --     liftIO $ payForVoucher database voucher
    --     postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 200
    --     postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 200

    -- it "receive 400 (OK) when the voucher is paid and previously redeemed with different tokens" $
    --   property $ \(voucher :: Voucher) (firstTokens :: [BlindedToken]) (secondTokens :: [BlindedToken]) ->
    --   do
    --     liftIO $ payForVoucher database voucher
    --     postJSON path (encode $ Redeem voucher firstTokens) `shouldRespondWith` 200
    --     postJSON path (encode $ Redeem voucher secondTokens) `shouldRespondWith` 400

matchJSONBody :: Result -> MatchBody
matchJSONBody expected =
  let
    bodyMatcher :: [Header] -> Body -> Maybe String
    bodyMatcher headers actualBody =
      case decode actualBody of
        Nothing ->
          Just $ "failed to decode body as value of expected type: " ++ show actualBody
        Just actual ->
          if actual == expected then
            Nothing
          else
            Just $ "decoded body does not equal expected value: " ++ show actual ++ show expected
  in
    MatchBody bodyMatcher
