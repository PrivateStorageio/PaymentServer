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
import Util.Spec
  ( wrongMethodNotAllowed
  , nonJSONUnsupportedMediaType
  , wrongJSONInvalidRequest
  )
import PaymentServer.Redemption
  ( RedemptionAPI
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

spec_db :: Spec
spec_db = do
  around (withConnection memory) $ do
    describe "redemptionServer" $ do
      it "responds to redemption of an unpaid voucher with 400 (Invalid Request)" $
        \(db :: MemoryVoucherDatabase) -> do
          payForVoucher db "abcdefg"
