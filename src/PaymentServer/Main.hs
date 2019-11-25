{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements the main entrypoint to the PaymentServer.
module PaymentServer.Main
  ( main
  ) where

import Control.Exception.Base
  ( SomeException
  )
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
import Network.HTTP.Types.Status
  ( status500
  )
import Network.Wai.Handler.Warp
  ( Port
  , defaultSettings
  , setPort
  , setOnException
  , setOnExceptionResponse
  , runSettings
  )
import Network.Wai.Handler.WarpTLS
  ( runTLS
  , tlsSettingsChain
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , responseLBS
  )
import Network.Wai.Middleware.Cors
  ( Origin
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
  , many
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
  , (<|>)
  )
import System.Exit
  ( exitFailure
  )
import Data.Semigroup ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as LBS

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
  , signingKeyPath  :: Maybe FilePath
  , database        :: Database
  , databasePath    :: Maybe Text
  , endpoint        :: Endpoint
  , stripeKeyPath   :: FilePath
  , corsOrigins     :: [Origin]
  }
  deriving (Show, Eq)

-- | An Endpoint represents the configuration for a socket's IP address.
-- There are some layering violations here.  I'm just copying Twisted
-- endpoints at the moment.  At some point it would be great to implement a
-- general purpose endpoint library outside of PaymentServer and without the
-- layering violations.
data Endpoint =
  -- | A TCPEndpoint represents a bare TCP/IP socket address.
  TCPEndpoint
  { portNumber :: Port
  }
  |
  -- | A TLSEndpoint represents a TCP/IP socket address which will have TLS
  -- used over it.
  TLSEndpoint
  { portNumber      :: Port
  , certificatePath :: FilePath
  , chainPath       :: Maybe FilePath
  , keyPath         :: FilePath
  }
  deriving (Show, Eq)

http :: Parser Endpoint
http = TCPEndpoint
  <$> option auto
  ( long "http-port"
    <> help "Port number on which to accept HTTP connections."
  )

https :: Parser Endpoint
https = TLSEndpoint
  <$> option auto
  ( long "https-port"
    <> help "Port number on which to accept HTTPS connections." )
  <*> strOption
  ( long "https-certificate-path"
    <> help "Filesystem path to the TLS certificate to use for HTTPS." )
  <*> optional
  ( strOption
    ( long "https-certificate-chain-path"
      <> help "Filesystem path to the TLS certificate chain to use for HTTPS." ) )
  <*> strOption
  ( long "https-key-path"
    <> help "Filesystem path to the TLS private key to use for HTTPS." )


sample :: Parser ServerConfig
sample = ServerConfig
  <$> option auto
  ( long "issuer"
    <> help "Which issuer to use: trivial or ristretto"
    <> showDefault
    <> value Trivial )
  <*> optional (option str
  ( long "signing-key-path"
    <> help "Path to base64 encoded signing key (ristretto only)"
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
  <*> (http <|> https)
  <*> option str
  ( long "stripe-key-path"
    <> help "Path to Stripe Secret key" )
  <*> many ( option str
             ( long "cors-origin"
             <> help "An allowed `Origin` for the purposes of CORS (zero or more)." ) )

opts :: ParserInfo ServerConfig
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc ""
  <> header  ""
  )

main :: IO ()
main = do
    config <- execParser opts
    app <- getApp config
    let run = getRunner (endpoint config)
    logEndpoint (endpoint config)
    run app

getPortNumber (TCPEndpoint portNumber) = portNumber
getPortNumber (TLSEndpoint portNumber _ _ _) = portNumber

getRunner :: Endpoint -> (Application -> IO ())
getRunner endpoint =
  let
    onException :: Maybe Request -> SomeException -> IO ()
    onException _ exc = do
      print "onException"
      print exc
      return ()
    onExceptionResponse :: SomeException -> Response
    onExceptionResponse = (responseLBS status500 []) . LBS.fromString . ("exception: " ++) . show
    settings =
      setPort (getPortNumber endpoint) .
      setOnException onException .
      setOnExceptionResponse onExceptionResponse $
      defaultSettings
  in
    case endpoint of
      (TCPEndpoint _) ->
        runSettings settings
      (TLSEndpoint _ certificatePath chainPath keyPath) ->
        let
          tlsSettings = tlsSettingsChain certificatePath (maybeToList chainPath) keyPath
        in
          runTLS tlsSettings settings

logEndpoint :: Endpoint -> IO ()
logEndpoint endpoint =
  case endpoint of
    TCPEndpoint { portNumber } ->
      putStrLn (printf "Accepting HTTP connections on %d" portNumber :: String)
    TLSEndpoint { portNumber } ->
      putStrLn (printf "Accepting HTTPS connections on %d" portNumber :: String)

getApp :: ServerConfig -> IO Application
getApp config =
  let
    getIssuer ServerConfig{ issuer, signingKeyPath } =
      case (issuer, signingKeyPath) of
        (Trivial, Nothing) -> return $ Right trivialIssue
        (Ristretto, Just keyPath) -> do
          key <- TIO.readFile keyPath
          return $ Right $ ristrettoIssue key
        _ -> return $ Left "invalid options"
    getDatabase ServerConfig{ database, databasePath } =
      case (database, databasePath) of
        (Memory, Nothing) -> Right memory
        (SQLite3, Just path) -> Right (getDBConnection path)
        _ -> Left "invalid options"
  in do
    issuer <- getIssuer config
    case issuer of
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
            key <- B.readFile (stripeKeyPath config)
            let
              origins = corsOrigins config
              app = paymentServerApp origins key issuer db
            logger <- mkRequestLogger (def { outputFormat = Detailed True})
            return $ logger app
