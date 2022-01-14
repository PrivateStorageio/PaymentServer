-- | Extract a public key from Ristretto-flavored PrivacyPass signing key read from stdin.
module Main
  ( main
  ) where

import Prelude hiding
  ( putStrLn
  , getLine
  )

import Data.Text.IO
  ( putStrLn
  , getLine
  )

import PaymentServer.Ristretto
  ( getPublicKey
  )

main :: IO ()
main = getLine >>= getPublicKey >>= putStrLn
