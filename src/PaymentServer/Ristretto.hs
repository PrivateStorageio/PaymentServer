{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( Issuance(Issuance)
  , randomSigningKey
  , getPublicKey
  , ristretto
  ) where

import Control.Exception
  ( bracket
  )
import Control.Monad.Except
  ( ExceptT
  , throwError
  , runExceptT
  , liftIO
  )

import System.IO.Unsafe
  ( unsafePerformIO
  )
import Data.Text
  ( Text
  , unpack
  , pack
  )

import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
import Foreign.C.String
  ( CString
  , newCString
  , peekCString
  )
import Foreign.Marshal.Alloc
  ( free
  )
import Foreign.Marshal.Array
  ( withArray
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
foreign import ccall "signing_key_encode_base64" signing_key_encode_base64 :: Ptr C_SigningKey -> IO CString
foreign import ccall "signing_key_destroy" signing_key_destroy :: Ptr C_SigningKey -> IO ()
foreign import ccall "signing_key_get_public_key" signing_key_get_public_key :: Ptr C_SigningKey -> IO (Ptr C_PublicKey)
foreign import ccall "signing_key_sign" signing_key_sign :: Ptr C_SigningKey -> Ptr C_BlindedToken -> IO (Ptr C_SignedToken)

foreign import ccall "signed_token_encode_base64" signed_token_encode_base64 :: Ptr C_SignedToken -> IO CString

foreign import ccall "batch_dleq_proof_new" batch_dleq_proof_new :: Ptr (Ptr C_BlindedToken) -> Ptr (Ptr C_SignedToken) -> Int -> Ptr C_SigningKey -> IO (Ptr C_BatchDLEQProof)
foreign import ccall "batch_dleq_proof_encode_base64" batch_dleq_proof_encode_base64 :: Ptr C_BatchDLEQProof -> IO CString
foreign import ccall "batch_dleq_proof_destroy" batch_dleq_proof_destroy :: Ptr C_BatchDLEQProof -> IO ()

-- | Private type to represent the return value of ristretto.
data Issuance =
  Issuance
  { publicKey  :: Text   -- ^ The base64-encoded public key corresponding to the
                         -- signing key which generated the signatures.
  , signatures :: [Text] -- ^ A list of base64-encoded token signatures.
  , proof      :: Text   -- ^ The base64-encoded batch DLEQ proof that the signatures
                         -- were made with the signing key corresponding to the public
                         -- key.
  }

data RistrettoFailure
  = SigningKeyAllocation
  | SigningKeyDecoding
  | BlindedTokenAllocation
  | BlindedTokenDecoding
  | SignedTokenAllocation
  | TokenSigning
  | SignedTokenEncoding
  | ProofCreation
  | SignedTokenPeek
  | PublicKeyLookup
  | PublicKeyEncoding
  deriving (Show, Eq)

nullIsError :: b -> IO (Ptr a) -> ExceptT b IO (Ptr a)
nullIsError fallback op =
  liftIO op >>= \r ->
                  case r of
                    nullPtr -> throwError fallback
                    ptr -> return ptr

anyNullIsError :: b -> IO [Ptr a] -> ExceptT b IO [Ptr a]
anyNullIsError fallback op =
  liftIO op >>= \r ->
                  if nullPtr `elem` r
                  then throwError fallback
                  else return r

ristretto
  :: Text                                  -- ^ The base64 encoded signing key.
  -> [Text]                                -- ^ A list of the base64 blinded tokens.
  -> Either RistrettoFailure Issuance      -- ^ Left for an error, otherwise
                                           -- Right with the ristretto results
ristretto textSigningKey textTokens =
  let
    newProof blindedTokens signedTokens signingKey =
      withArray blindedTokens $ \cBlindedTokensArray ->
      withArray signedTokens $ \cSignedTokensArray ->
      batch_dleq_proof_new cBlindedTokensArray cSignedTokensArray (length blindedTokens) signingKey

    newEncodedProof blindedTokens signedTokens signingKey =
      bracket (newProof blindedTokens signedTokens signingKey) batch_dleq_proof_destroy $ \proof ->
      bracket (batch_dleq_proof_encode_base64 proof) free peekCString

    stringSigningKey = unpack textSigningKey
    stringTokens = map unpack textTokens

    extractKeyMaterial :: String -> ExceptT RistrettoFailure IO (Ptr C_SigningKey, Ptr C_PublicKey)
    extractKeyMaterial stringSigningKey = do
      cStringSigningKey <- nullIsError SigningKeyAllocation $ newCString stringSigningKey
      signingKey <- nullIsError SigningKeyDecoding $ signing_key_decode_base64 cStringSigningKey
      publicKey <- nullIsError PublicKeyLookup $ signing_key_get_public_key signingKey
      return (signingKey, publicKey)
  in
    unsafePerformIO . runExceptT $ do
      (signingKey, publicKey) <- extractKeyMaterial stringSigningKey
      cStringEncodedPublicKey <- nullIsError PublicKeyEncoding $ public_key_encode_base64 publicKey
      encodedPublicKey <- liftIO $ peekCString cStringEncodedPublicKey
      cStringTokens <- anyNullIsError BlindedTokenAllocation $ mapM newCString stringTokens
      blindedTokens <- anyNullIsError BlindedTokenDecoding $ mapM blinded_token_decode_base64 cStringTokens
      signedTokens <- anyNullIsError TokenSigning $ mapM (signing_key_sign signingKey) blindedTokens
      encodedCStringSignedTokens <- anyNullIsError SignedTokenEncoding $ mapM signed_token_encode_base64 signedTokens
      encodedSignedTokens <- liftIO $ mapM peekCString encodedCStringSignedTokens
      encodedProof <- liftIO $ newEncodedProof blindedTokens signedTokens signingKey
      return $ Issuance (pack encodedPublicKey) (map pack encodedSignedTokens) (pack encodedProof)


-- | randomSigningKey generates a new signing key at random and returns it
-- encoded as a base64 string.
randomSigningKey :: IO Text
randomSigningKey = do
  cSigningKey <- signing_key_random
  cString <- signing_key_encode_base64 cSigningKey
  signing_key_destroy cSigningKey
  result <- peekCString cString
  free cString
  return $ pack result

-- | getPublicKey returns the base64 encoded public key corresponding to the
-- base64 encoded signing key passed to it.
getPublicKey :: Text -> IO Text
getPublicKey enc_skey = do
  enc_cstr_skey <- newCString . unpack $ enc_skey
  skey <- signing_key_decode_base64 $ enc_cstr_skey
  pkey <- signing_key_get_public_key skey
  enc_cstr_pkey <- public_key_encode_base64 pkey
  enc_pkey <- peekCString enc_cstr_pkey
  return $ pack enc_pkey
