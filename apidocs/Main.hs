module Main
  ( main
  ) where

import PaymentServer.Server
  ( paymentServerAPI
  )
import Servant.Docs
  ( API
  , docs
  , markdown
  )

-- Generate the Documentation's ADT
greetDocs :: API
greetDocs = docs paymentServerAPI

main :: IO ()
main = print $ markdown greetDocs
