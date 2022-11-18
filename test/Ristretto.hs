{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests related to PaymentServer.Ristretto.

module Ristretto
  ( tests
  ) where

import Test.Tasty
  ( TestTree
  , testGroup
  )

import Test.Tasty.HUnit
  ( testCase
  , assertEqual
  , assertFailure
  )

import PaymentServer.Ristretto
  ( Issuance(Issuance, publicKey, signatures)
  , ristretto
  , randomSigningKey
  , randomToken
  , blindToken
  , getPublicKey
  )

tests :: TestTree
tests = testGroup "Ristretto"
  [ issueTests
  ]

issueTests :: TestTree
issueTests = testGroup "Issuance"
  [ testCase "ristretto returns an issuance" $ do
      key <- randomSigningKey
      expectedPublicKey <- getPublicKey key
      aToken <- randomToken
      aBlindToken <- blindToken aToken
      let blindedTokens = [aBlindToken]
      result <- ristretto key blindedTokens
      case result of
        (Right Issuance { publicKey, signatures }) -> do
          assertEqual "The public key matches the signing key." expectedPublicKey publicKey
          assertEqual "The number of signatures equals the number of tokens." (length blindedTokens) (length signatures)
          -- XXX The proof checks out
        Left err -> do
          assertFailure $ show err
  ]
