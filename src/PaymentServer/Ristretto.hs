{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( randomSigningKey
  ) where

import Data.Text
  ( Text
  , pack
  )

import Foreign.Ptr
  ( Ptr
  )
import Foreign.C.String
  ( peekCString
  )
import Foreign.Marshal.Alloc
  ( free
  )

data C_SigningKey

foreign import ccall "signing_key_random" signing_key_random :: IO (Ptr C_SigningKey)

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
