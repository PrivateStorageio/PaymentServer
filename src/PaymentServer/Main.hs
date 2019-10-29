{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements the main entrypoint to the PaymentServer.
module PaymentServer.Main
  ( main
  ) where

import Text.Printf
  ( printf
  )
import Data.Maybe
  ( maybeToList
  )
import Data.Text
  ( Text
  )
import Data.Default
  ( def
  )
import Network.Wai.Handler.Warp
  ( defaultSettings
  , setPort
  )
import Network.Wai.Handler.WarpTLS
  ( TLSSettings
  , runTLS
  , tlsSettingsChain
  )
import Network.Wai
  ( Application
  )
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(Detailed)
  , outputFormat
  , mkRequestLogger
  )
import PaymentServer.Persistence
  ( memory
  , getDBConnection
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
  , strOption
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

data Database =
  Memory
  | SQLite3
  deriving (Show, Eq, Ord, Read)

data ServerConfig = ServerConfig
  { issuer          :: Issuer
  , signingKey      :: Maybe Text
  , database        :: Database
  , databasePath    :: Maybe Text
  , httpPortNumber  :: Int
  , certificatePath :: String
  , chainPath       :: Maybe String
  , keyPath         :: String
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
    <> value Memory )
  <*> optional ( option str
  ( long "database-path"
    <> help "Path to on-disk database (sqlite3 only)"
    <> showDefault ) )
  <*> option auto
  ( long "https-port"
    <> help "Port number on which to accept HTTPS connections."
    <> showDefault
    <> value 443 )
  <*> strOption
  ( long "https-certificate-path"
    <> help "Filesystem path to the TLS certificate to use for HTTPS." )
  <*> optional ( strOption
  ( long "https-certificate-chain-path"
    <> help "Filesystem path to the TLS certificate chain to use for HTTPS." ) )
  <*> strOption
  ( long "https-key-path"
    <> help "Filesystem path to the TLS private key to use for HTTPS." )


opts :: ParserInfo ServerConfig
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc ""
  <> header  ""
  )

main :: IO ()
main = do
    config <- execParser opts
    let port = httpPortNumber config
    app <- getApp config
    tlsSettings <- getTlsSettings config
    putStrLn (printf "Accepting HTTPS connections on %d" port :: String)
    runTLS tlsSettings (setPort port defaultSettings) app

getTlsSettings :: ServerConfig -> IO TLSSettings
getTlsSettings ServerConfig{ certificatePath, chainPath, keyPath } =
  return $ tlsSettingsChain certificatePath (maybeToList chainPath) keyPath

getApp :: ServerConfig -> IO Application
getApp config =
  let
    getIssuer ServerConfig{ issuer, signingKey } =
      case (issuer, signingKey) of
        (Trivial, Nothing) -> Right trivialIssue
        (Ristretto, Just key) -> Right $ ristrettoIssue key
        _ -> Left "invalid options"
    getDatabase ServerConfig{ database, databasePath } =
      case (database, databasePath) of
        (Memory, Nothing) -> Right memory
        (SQLite3, Just path) -> Right (getDBConnection path)
        _ -> Left "invalid options"
  in do
    case getIssuer config of
      Left err -> do
        print err
        exitFailure
      Right issuer ->
        case getDatabase config of
          Left err -> do
            print err
            exitFailure
          Right getDB -> do
            db <- getDB
            let app = paymentServerApp issuer db
            logger <- mkRequestLogger (def { outputFormat = Detailed True})
            return $ logger app
