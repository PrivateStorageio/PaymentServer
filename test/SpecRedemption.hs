{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite related to voucher redemption.

module SpecRedemption where

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
  ( with
  , shouldRespondWith
  , liftIO
  )
import Test.Hspec.Wai.QuickCheck
  ( property
  )
import Test.QuickCheck
  ( (==>)
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
  ( Voucher
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

spec_simple :: Spec
spec_simple = with (app <$> memory) $ parallel $ do
  describe (printf "error behavior of POST %s" (show path)) $ do
    wrongMethodNotAllowed "GET" path
    nonJSONUnsupportedMediaType path
    wrongJSONInvalidRequest path "{}"

withConnection :: VoucherDatabase d => IO d -> ((d -> IO ()) -> IO ())
withConnection getDB = bracket getDB (\db -> return ())

make_spec_db :: VoucherDatabase d => IO d -> Spec
make_spec_db getDatabase = do
  -- Create the database so we can interact with it directly in the tests
  -- below.
  database <- runIO getDatabase
  before (return $ app database) $
    describe "redemption attempts on the server" $ do
    it "receive 400 (Invalid Request) when the voucher is unpaid" $
      property $ \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
      postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 400

    it "receive 200 (OK) when the voucher is paid" $
      property $ \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
      do
        liftIO $ payForVoucher database voucher
        postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 200

    it "receive 200 (OK) when the voucher is paid and previously redeemed with the same tokens" $
      property $ \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
      do
        liftIO $ payForVoucher database voucher
        postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 200
        postJSON path (encode $ Redeem voucher tokens) `shouldRespondWith` 200

    it "receive 400 (OK) when the voucher is paid and previously redeemed with different tokens" $
      property $ \(voucher :: Voucher) (firstTokens :: [BlindedToken]) (secondTokens :: [BlindedToken]) ->
      do
        liftIO $ payForVoucher database voucher
        postJSON path (encode $ Redeem voucher firstTokens) `shouldRespondWith` 200
        postJSON path (encode $ Redeem voucher secondTokens) `shouldRespondWith` 400




spec_memory_db :: Spec
spec_memory_db =
  make_spec_db memory
