-- | This module implements the main entrypoint to the PaymentServer.
module PaymentServer.Main
  ( main
  ) where

import Data.Default
  ( def
  )
import Network.Wai.Handler.Warp
  ( run
  )
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(Detailed)
  , outputFormat
  , mkRequestLogger
  )
import PaymentServer.Persistence
  ( memory
  )
import PaymentServer.Server
  ( paymentServerApp
  )

main :: IO ()
main = do
  db <- memory
  let app = paymentServerApp db
  logger <- mkRequestLogger $ def { outputFormat = Detailed True}
  run 8081 $ logger app
