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

import Options.Applicative
  ( ParserInfo
  , execParser
  , info
  , helper
  , header
  , fullDesc
  , progDesc
  , (<**>)
  )

import PaymentServer.Ristretto
  ( getPublicKey
  )

opts :: ParserInfo ()
opts = info (pure () <**> helper)
  ( fullDesc
    <> progDesc "The private key is read from stdin."
    <> header "Derive the public key for a Ristretto private key"
  )

main :: IO ()
main =
  execParser opts >> getLine >>= getPublicKey >>= putStrLn
