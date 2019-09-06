{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( ristretto
  ) where

import Data.Text
  ( Text
  , unpack
  )

import Foreign.Ptr
  ( Ptr
  )
import Foreign.C.String
  ( CString
  , withCString
  , newCString
  )

data C_BlindedToken
data C_SignedToken
data C_SigningKey
data C_PublicKey
data C_BatchDLEQProof

foreign import ccall "blinded_token_decode_base64" blinded_token_decode_base64 :: CString -> IO (Ptr C_BlindedToken)
foreign import ccall "blinded_token_destroy" blinded_token_destroy :: Ptr C_BlindedToken -> IO ()

foreign import ccall "public_key_encode_base64" public_key_encode_base64 :: Ptr C_PublicKey -> IO CString

foreign import ccall "signing_key_random" signing_key_random :: IO (Ptr C_SigningKey)
foreign import ccall "signing_key_decode_base64" signing_key_decode_base64 :: CString -> IO (Ptr C_SigningKey)
foreign import ccall "signing_key_destroy" signing_key_destroy :: Ptr C_SigningKey -> IO ()
foreign import ccall "signing_key_get_public_key" signing_key_get_public_key :: Ptr C_SigningKey -> IO (Ptr C_PublicKey)
foreign import ccall "signing_key_sign" signing_key_sign :: Ptr C_SigningKey -> Ptr C_BlindedToken -> IO (Ptr C_SignedToken)

foreign import ccall "signed_token_encode_base64" signed_token_encode_base64 :: Ptr C_SignedToken -> IO CString

foreign import ccall "batch_dleq_proof_new" batch_dleq_proof_new :: Ptr C_BlindedToken -> Ptr C_SignedToken -> Int -> Ptr C_SigningKey -> IO (Ptr C_BatchDLEQProof)
foreign import ccall "batch_dleq_proof_encode_base64" batch_dleq_proof_encode_base64 :: Ptr C_BatchDLEQProof -> IO CString
foreign import ccall "batch_dleq_proof_destroy" batch_dleq_proof_destroy :: Ptr C_BatchDLEQProof -> IO ()

ristretto
  :: Text                       -- ^ The base64 encoded signing key.
  -> [Text]                     -- ^ A list of the base64 blinded tokens.
  -> IO (Text, [Text], Text)    -- ^ The base64 public key, list of base64 signed tokens, and the base64 proof.
ristretto textSigningKey textTokens = do
  let stringSigningKey = unpack textSigningKey
  cStringSigningKey <- newCString stringSigningKey
  signingKey <- signing_key_decode_base64 cStringSigningKey
  let stringTokens = map unpack textTokens
  cStringTokens <- mapM newCString stringTokens
  blindedTokens <- mapM blinded_token_decode_base64 cStringTokens
  signedTokens <- mapM (signing_key_sign signingKey) blindedTokens
    -- encodedTokens <- map signed_token_encode_base64 signedTokens
    -- proof <- batch_dleq_proof_new blindedTokens signedTokens (length blindedTokens) signingKey
    -- encodedProof <- batch_dleq_proof_encode_base64 proof
    -- publicKey <- signing_key_get_public_key signingKey
    -- encodedPublicKey <- public_key_encode_base64 publicKey
    -- ChallengeBypass
    --   encodedPublicKey
    --   encodedTokens
    --   encodedProof
  return (mempty, [], mempty)
