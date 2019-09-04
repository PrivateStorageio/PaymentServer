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
import Control.Exception
  ( bracket
  )
import Data.Aeson
 ( encode
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
  , before
  , around
  , shouldReturn
  , shouldBe
  , runIO
  )
import Test.Hspec.Wai
  ( ResponseMatcher(ResponseMatcher)
  , WaiExpectation
  , with
  , shouldRespondWith
  , liftIO
  )
import Test.Hspec.Wai.QuickCheck
  ( property
  )
import Test.QuickCheck
  ( Property
  , (==>)
  )
import Test.QuickCheck.Monadic
  ( pre
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
import PaymentServer.Redemption
  ( RedemptionAPI
  , BlindedToken
  , Redeem(Redeem)
  , redemptionServer
  )
import PaymentServer.Persistence
  ( RedeemError(NotPaid)
  , Voucher
  , Fingerprint
  , VoucherDatabase(payForVoucher, redeemVoucher)
  , MemoryVoucherDatabase
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
      it "receives 400 (Invalid Request) when the voucher is not paid" $ property $
      \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
        propertyRedeem path voucher tokens 400

    with (return $ app PermitRedemption) $
      it "receive 200 (OK) when redemption succeeds" $ property $
      \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
        propertyRedeem path voucher tokens 200

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
