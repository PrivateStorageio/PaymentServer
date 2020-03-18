{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( Issuance(Issuance)
  , randomSigningKey
  , ristretto
  ) where

import Control.Exception
  ( bracket
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

    extractKeyMaterial :: String -> IO (Either RistrettoFailure (Ptr C_SigningKey, Ptr C_PublicKey))
    extractKeyMaterial stringSigningKey = do
      cStringSigningKey <- newCString stringSigningKey
      if cStringSigningKey == nullPtr
        then return $ Left SigningKeyAllocation
        else do
          signingKey <- signing_key_decode_base64 cStringSigningKey
          if signingKey == nullPtr
            then return $ Left SigningKeyDecoding
            else do
              publicKey <- signing_key_get_public_key signingKey
              if publicKey == nullPtr
                then return $ Left PublicKeyLookup
                else return $ Right (signingKey, publicKey)
  in
    unsafePerformIO $ do
      keys <- extractKeyMaterial stringSigningKey
      case keys of
        Left err -> return $ Left err
        Right (signingKey, publicKey) -> do
          cStringEncodedPublicKey <- public_key_encode_base64 publicKey
          if cStringEncodedPublicKey == nullPtr
            then return $ Left PublicKeyEncoding
            else do
              encodedPublicKey <- peekCString cStringEncodedPublicKey
              cStringTokens <- mapM newCString stringTokens
              if nullPtr `elem` cStringTokens
                then return $ Left BlindedTokenAllocation
                else do
                  blindedTokens <- mapM blinded_token_decode_base64 cStringTokens
                  if nullPtr `elem` blindedTokens
                    then return $ Left BlindedTokenDecoding
                    else do
                      signedTokens <- mapM (signing_key_sign signingKey) blindedTokens
                      if nullPtr `elem` signedTokens
                        then return $ Left TokenSigning
                        else do
                          encodedCStringSignedTokens <- mapM signed_token_encode_base64 signedTokens
                          if nullPtr `elem` encodedCStringSignedTokens
                            then return $ Left SignedTokenEncoding
                            else do
                              encodedSignedTokens <- mapM peekCString encodedCStringSignedTokens
                              encodedProof <- newEncodedProof blindedTokens signedTokens signingKey
                              return . Right $ Issuance (pack encodedPublicKey) (map pack encodedSignedTokens) (pack encodedProof)


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
