-- | Generate a random Ristretto-flavored PrivacyPass signing key.
module Main
  ( main
  ) where

import Prelude hiding
  ( putStrLn
  )

import Data.Text.IO
  ( putStrLn
  )

import PaymentServer.Ristretto
  ( randomSigningKey
  )

main :: IO ()
main = randomSigningKey >>= putStrLn
