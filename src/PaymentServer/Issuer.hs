{-# LANGUAGE OverloadedStrings #-}

-- | This module can issue signatures of blinded tokens which can be used to
-- construct passes.
module PaymentServer.Issuer
  ( PublicKey
  , Signature
  , BlindedToken
  , Proof
  , ChallengeBypass(ChallengeBypass)
  , Issuer
  , trivialIssue
  ) where

import Data.Text
  ( Text
  )

-- | A public key corresponding to our private key.
type PublicKey = Text

-- | A cryptographic signature of a blinded token created using our private
-- key.
type Signature = Text

-- | This is the blinded token for which we create signatures.
type BlindedToken = Text

-- | A zero-knowledge proof that signatures were created of the corresponding
-- blinded tokens using the corresponding public key's private key.
type Proof = Text

-- | This bundles up all of the values needed to verify the privacy-respecting
-- operation of the issuer and then construct passes.
data ChallengeBypass =
  ChallengeBypass PublicKey [Signature] Proof

-- | An issuer accepts a list of blinded tokens and returns signatures of
-- those tokens along with proof that it used a particular key to construct
-- the signatures.
type Issuer = [BlindedToken] -> ChallengeBypass

-- | trivialIssue makes up and returns some nonsense values that satisfy the
-- structural requirements but not the semantic ones.
trivialIssue :: Issuer
trivialIssue tokens =
  ChallengeBypass
  "fake-public-key"
  (replicate (length tokens) "fake-signature")
  "fake-proof"
