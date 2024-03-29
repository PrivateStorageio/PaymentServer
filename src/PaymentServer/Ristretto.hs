{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( Issuance(Issuance, publicKey, signatures, proof)
  , randomSigningKey
  , randomToken
  , blindToken
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
  , newCAString
  , peekCAString
  , withCAString
  )
import Foreign.Marshal.Alloc
  ( free
  )
import Foreign.Marshal.Array
  ( withArray
  )

data C_Token
data C_BlindedToken
data C_SignedToken
data C_SigningKey
data C_PublicKey
data C_BatchDLEQProof

foreign import ccall "blinded_token_encode_base64" blinded_token_encode_base64 :: Ptr C_BlindedToken -> IO CString
foreign import ccall "blinded_token_decode_base64" blinded_token_decode_base64 :: CString -> IO (Ptr C_BlindedToken)
foreign import ccall "blinded_token_destroy" blinded_token_destroy :: Ptr C_BlindedToken -> IO ()

foreign import ccall "public_key_encode_base64" public_key_encode_base64 :: Ptr C_PublicKey -> IO CString
foreign import ccall "public_key_destroy" public_key_destroy :: Ptr C_PublicKey -> IO ()

foreign import ccall "signing_key_random" signing_key_random :: IO (Ptr C_SigningKey)
foreign import ccall "signing_key_decode_base64" signing_key_decode_base64 :: CString -> IO (Ptr C_SigningKey)
foreign import ccall "signing_key_encode_base64" signing_key_encode_base64 :: Ptr C_SigningKey -> IO CString
foreign import ccall "signing_key_destroy" signing_key_destroy :: Ptr C_SigningKey -> IO ()
foreign import ccall "signing_key_get_public_key" signing_key_get_public_key :: Ptr C_SigningKey -> IO (Ptr C_PublicKey)
foreign import ccall "signing_key_sign" signing_key_sign :: Ptr C_SigningKey -> Ptr C_BlindedToken -> IO (Ptr C_SignedToken)

foreign import ccall "token_random" token_random :: IO (Ptr C_Token)
foreign import ccall "token_blind" token_blind :: Ptr C_Token -> IO (Ptr C_BlindedToken)
foreign import ccall "token_destroy" token_destroy :: Ptr C_Token -> IO ()
foreign import ccall "token_encode_base64" token_encode_base64 :: Ptr C_Token -> IO CString
foreign import ccall "token_decode_base64" token_decode_base64 :: CString -> IO (Ptr C_Token)

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
                  if r == nullPtr
                  then throwError fallback
                  else return r

anyNullIsError :: b -> IO [Ptr a] -> ExceptT b IO [Ptr a]
anyNullIsError fallback op =
  liftIO op >>= \r ->
                  if nullPtr `elem` r
                  then throwError fallback
                  else return r

ristretto
  :: Text                                  -- ^ The base64 encoded signing key.
  -> [Text]                                -- ^ A list of the base64 blinded tokens.
  -> IO (Either RistrettoFailure Issuance) -- ^ Left for an error, otherwise
                                           -- Right with the ristretto results
ristretto textSigningKey textTokens =
  let
    newProof blindedTokens signedTokens signingKey =
      withArray blindedTokens $ \cBlindedTokensArray ->
      withArray signedTokens $ \cSignedTokensArray ->
      batch_dleq_proof_new cBlindedTokensArray cSignedTokensArray (length blindedTokens) signingKey

    newEncodedProof blindedTokens signedTokens signingKey =
      bracket (newProof blindedTokens signedTokens signingKey) batch_dleq_proof_destroy $ \proof ->
      bracket (batch_dleq_proof_encode_base64 proof) free peekCAString

    stringSigningKey = unpack textSigningKey
    stringTokens = map unpack textTokens
  in
    runExceptT $
    nullIsError SigningKeyAllocation (newCAString stringSigningKey) >>= \cStringSigningKey ->
    nullIsError SigningKeyDecoding (signing_key_decode_base64 cStringSigningKey) >>= \signingKey ->
    nullIsError PublicKeyLookup (signing_key_get_public_key signingKey) >>= \publicKey ->
    nullIsError PublicKeyEncoding (public_key_encode_base64 publicKey) >>= \cStringEncodedPublicKey ->
    liftIO (peekCAString cStringEncodedPublicKey) >>= \encodedPublicKey ->
    anyNullIsError BlindedTokenAllocation (mapM newCAString stringTokens) >>= \cStringTokens ->
    anyNullIsError BlindedTokenDecoding (mapM blinded_token_decode_base64 cStringTokens) >>= \blindedTokens ->
    anyNullIsError TokenSigning (mapM (signing_key_sign signingKey) blindedTokens) >>= \signedTokens ->
    anyNullIsError SignedTokenEncoding (mapM signed_token_encode_base64 signedTokens) >>= \encodedCStringSignedTokens ->
    liftIO (mapM peekCAString encodedCStringSignedTokens) >>= \encodedSignedTokens ->
    liftIO (newEncodedProof blindedTokens signedTokens signingKey) >>= \encodedProof ->
    return $ Issuance (pack encodedPublicKey) (map pack encodedSignedTokens) (pack encodedProof)


-- | randomSigningKey generates a new signing key at random and returns it
-- encoded as a base64 string.
randomSigningKey :: IO Text
randomSigningKey =
  bracket signing_key_random signing_key_destroy $ \cSigningKey ->
  bracket (signing_key_encode_base64 cSigningKey) free $ \cString ->
  pack <$> peekCAString cString

-- | randomToken generates a new token at random and returns it encoded as a
-- base64 string.
randomToken :: IO Text
randomToken =
  bracket token_random token_destroy $ \cToken ->
  bracket (token_encode_base64 cToken) free $ \cString ->
  pack <$> peekCAString cString

-- | blindToken takes a token encoded as base64 and returns the base64
-- encoding of the blinding of the token.
blindToken :: Text -> IO Text
blindToken token =
  withCAString (unpack token) $ \cString ->
  bracket (token_decode_base64 cString) token_destroy $ \cToken ->
  bracket (token_blind cToken) blinded_token_destroy $ \cBlinded ->
  bracket (blinded_token_encode_base64 cBlinded) free $ \cBlindedString ->
  pack <$> peekCAString cBlindedString

-- | getPublicKey returns the base64 encoded public key corresponding to the
-- base64 encoded signing key passed to it.
getPublicKey :: Text -> IO Text
getPublicKey enc_skey =
  withCAString (unpack enc_skey) $ \enc_cstr_skey ->
  bracket (signing_key_decode_base64 enc_cstr_skey) signing_key_destroy $ \skey ->
  bracket (signing_key_get_public_key skey) public_key_destroy $ \pkey ->
  bracket (public_key_encode_base64 pkey) free $ \enc_cstr_pkey ->
  pack <$> peekCAString enc_cstr_pkey
