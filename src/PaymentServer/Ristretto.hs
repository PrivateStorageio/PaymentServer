{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module PaymentServer.Ristretto
  ( randomSigningKey
  ) where

import Data.Text
  ( Text
  )

import Foreign.Ptr
  ( Ptr
  )

data C_SigningKey

foreign import ccall "signing_key_random" signing_key_random :: IO (Ptr C_SigningKey)

-- | randomSigningKey generates a new signing key at random and returns it
-- encoded as a base64 string.
randomSigningKey :: IO Text
randomSigningKey = do
  cSigningKey <- signing_key_random
  return $ ""
