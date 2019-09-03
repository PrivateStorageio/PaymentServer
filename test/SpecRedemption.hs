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
  )
import Test.Hspec.Wai
  ( with
  , post
  , shouldRespondWith
  , liftIO
  )
import Test.Hspec.Wai.QuickCheck
  ( property
  )
import Test.QuickCheck.Monadic
  ( assert
  )
import Test.QuickCheck.Instances.Text ()
import Util.Spec
  ( wrongMethodNotAllowed
  , nonJSONUnsupportedMediaType
  , wrongJSONInvalidRequest
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
make_spec_db getDatabase =
  before (getDatabase >>= return . app) $
  describe "redemptionServer" $
  it "responds to redemption of an unpaid voucher with 400 (Invalid Request)" $
  property $ \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
  do
    post path (encode $ Redeem voucher tokens) `shouldRespondWith` 400

spec_memory_db :: Spec
spec_memory_db =
  make_spec_db memory
