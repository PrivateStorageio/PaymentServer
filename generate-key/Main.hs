module Main
  ( main
  ) where

import PaymentServer.Ristretto
  ( randomSigningKey
  )

main :: IO ()
main = randomSigningKey >>= putStrLn
