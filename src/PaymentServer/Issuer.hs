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
  , ristrettoIssue
  ) where

import PaymentServer.Ristretto
  ( ristretto
  )

import Data.Text
  ( Text
  )

-- | A private key for signing.
type SigningKey = Text

-- | A public key corresponding to a SigningKey.
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
type Issuer = [BlindedToken] -> IO ChallengeBypass

-- | trivialIssue makes up and returns some nonsense values that satisfy the
-- structural requirements but not the semantic ones.
trivialIssue :: Issuer
trivialIssue tokens =
  return $
  ChallengeBypass
  "fake-public-key"
  (replicate (length tokens) "fake-signature")
  "fake-proof"

-- | ristrettoIssue uses Ristretto-flavored PrivacyPass (aka
-- `challenge-bypass-ristretto`) to create token signatures in a
-- privacy-preserving manner.
ristrettoIssue
  :: SigningKey    -- ^ The key to provide to the PrivacyPass signer.
  -> Issuer        -- ^ An issuer using the given key.
ristrettoIssue signingKey tokens = do
  (publicKey, tokens, proof) <- ristretto signingKey tokens
  return $ ChallengeBypass publicKey tokens proof
