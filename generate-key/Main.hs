-- | Generate a random Ristretto-flavored PrivacyPass signing key.
module Main
  ( main
  ) where

import qualified Data.Text.IO as TIO

import PaymentServer.Ristretto
  ( randomSigningKey
  )

main :: IO ()
main = randomSigningKey >>= TIO.putStrLn
