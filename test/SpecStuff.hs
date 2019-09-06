{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpecStuff where

import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.Wai
  ( assertBody
  , assertStatus
  , assertStatus'
  , get
  , post
  , testWai
  )
import Test.Tasty.QuickCheck
  ( testProperty
  )
import PaymentServer.Persistence
  ( Voucher
  )
import PaymentServer.Redemption
  ( RedemptionAPI
  , Result(Failed, Succeeded)
  )
import PaymentServer.Issuer
  ( BlindedToken
  )
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
import Test.QuickCheck
  ( ioProperty
  )
import Test.Hspec.Wai.QuickCheck
  ( Testable(toProperty)
  , WaiProperty(unWaiProperty)
  , property
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
  , Issuer
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

app :: VoucherDatabase d => Issuer -> d -> Application
app issue = serve redemptionAPI . redemptionServer issue

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

test_foo :: TestTree
test_foo = testGroup "Foo" redemption

redemption :: [TestTree]
redemption =
  [ testProperty "fails when the voucher is not paid" $
    \(voucher :: Voucher) (tokens :: [BlindedToken]) ->
      propertyRedeem path voucher tokens 400
      { matchBody = matchJSONBody Failed
      -- major/minor, fine.  charset=utf-8... okay.  but really this is
      -- overspecified by encoding the exact byte sequence.  I'd rather
      -- assert semantic equality.
      , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
      }
  ]

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
