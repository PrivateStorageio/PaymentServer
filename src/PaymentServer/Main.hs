{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements the main entrypoint to the PaymentServer.
module PaymentServer.Main
  ( main
  ) where

import Text.Printf
  ( printf
  )
import Data.Text
  ( Text
  )
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
  , Database(Memory, SQLite3)
  )
import PaymentServer.Issuer
  ( trivialIssue
  , ristrettoIssue
  )
import PaymentServer.Server
  ( paymentServerApp
  )

import Options.Applicative
  ( Parser
  , ParserInfo
  , option
  , auto
  , str
  , optional
  , long
  , help
  , value
  , showDefault
  , execParser
  , info
  , helper
  , fullDesc
  , progDesc
  , header
  , (<**>)
  )
import System.Exit
  ( exitFailure
  )
import Data.Semigroup ((<>))

data Issuer =
  Trivial
  | Ristretto
  deriving (Show, Eq, Ord, Read)

data ServerConfig = ServerConfig
  { issuer       :: Issuer
  , signingKey   :: Maybe Text
  , database     :: Database
  , databasePath :: Maybe Text
  }
  deriving (Show, Eq)

sample :: Parser ServerConfig
sample = ServerConfig
  <$> option auto
  ( long "issuer"
    <> help "Which issuer to use: trivial or ristretto"
    <> showDefault
    <> value Trivial )
  <*> optional (option str
  ( long "signing-key"
    <> help "The base64 encoded signing key (ristretto only)"
    <> showDefault ) )
  <*> option auto
  ( long "database"
    <> help "Which database to use: sqlite3 or memory"
    <> showDefault
    <> value SQLite3 )
  <*> optional ( option str
  ( long "database-path"
    <> help "Path to on-disk database (sqlite3 only)"
    <> showDefault ) )

opts :: ParserInfo ServerConfig
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc ""
  <> header  ""
  )

main :: IO ()
main =
  let
    getIssuer ServerConfig{ issuer, signingKey } =
      case (issuer, signingKey) of
        (Trivial, Nothing) -> Right trivialIssue
        (Ristretto, Just key) -> Right $ ristrettoIssue key
        _ -> Left "invalid options"
    getDatabase ServerConfig{ database, databasePath } =
      case (database, databasePath) of
        (Memory, Nothing) -> Right memory
        _ -> Left "invalid options"
  in do
    config <- execParser opts
    case getIssuer config of
      Left err -> do
        print err
        exitFailure
      Right issuer ->
        case getDatabase config of
          Left err ->do
            print err
            exitFailure
          Right getDB -> do
            db <- getDB
            let port = 8081
            let app = paymentServerApp issuer db
            logger <- mkRequestLogger (def { outputFormat = Detailed True})
            putStrLn (printf "Listening on %d" port :: String)
            run port $ logger app
