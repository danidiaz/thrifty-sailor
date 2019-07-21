{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.Main (defaultMain,ProviderName(..),tokenFromEnvironment) where

import           Prelude hiding (log)

import           Control.Monad.Except
import           Data.Coerce
import           Data.Foldable (for_)
import           Data.String (IsString(..))
import           Data.Text (Text)            
import qualified Data.Text             
import qualified Data.Text.IO             
import           Data.Map(Map)
import qualified Data.Map
import qualified Data.ByteString.Lazy
import           Data.Aeson
import           GHC.Generics 
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO (stdout)
import           Options.Applicative
import qualified Options.Applicative as O
import           GHC.Stack
import           Thrifty.Prelude
import           Thrifty.JSON
import           Thrifty

tokenFromEnvironment :: String -> (String -> SomeProvider) -> IO SomeProvider
tokenFromEnvironment variableName makeProvider =
  do token <- getEnv variableName
     return (makeProvider token)

newtype ProviderName = ProviderName Text deriving (Eq,Ord,Show,IsString,FromJSONKey)

newtype ServerName = ServerName Text deriving (Eq,Ord,Show,IsString,FromJSONKey)

data Command = 
      Providers
    | Candidates ProviderName
    | Status ProviderName ServerName
    | Up ProviderName ServerName
    | Down ProviderName ServerName
    deriving (Eq,Show)

data NameDesc = NameDesc { optionName :: String, optionDesc :: String }

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = 
            O.subparser 
          . mconcat
          $ [ 
              subcommand (pure Providers)  
                         (NameDesc "providers" 
                                   "Show available providers"),
              subcommand (Candidates <$> providerArgument)
                         (NameDesc "candidates" 
                                   "Show available candidates for snapshotification"),
              subcommand (Status <$> providerArgument <*> serverArgument)  
                         (NameDesc "status" 
                                   "Show current status of the target server"),
              subcommand (Up <$> providerArgument <*> serverArgument)
                         (NameDesc "up" 
                                   "Restores server from snapshot, deletes snapshot"),
              subcommand (Down <$> providerArgument <*> serverArgument)
                         (NameDesc "down" 
                                   "Shuts down server, makes snapshot, destroys server") ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p nd = 
        O.command (optionName nd) (infoHelpDesc p (optionDesc nd))

    providerArgument = strArgument (metavar "PROVIDER" <> help "Name of the provider")

    serverArgument = strArgument (metavar "SERVER" <> help "Name of the server")

defaultMain :: HasCallStack => [(ProviderName,IO SomeProvider)] -> IO ()
defaultMain (Data.Map.fromList -> plugins) = do
    command <- O.execParser parserInfo
    case command of
        Providers -> 
            do let providerNames = Data.Map.keys plugins
               for_ providerNames \(ProviderName name) -> Data.Text.IO.putStrLn name
        Candidates providerName -> 
            withSelectedProvider providerName \(Provider { candidates }) ->
              do cs <- candidates 
                 Data.ByteString.Lazy.hPut stdout (encode cs)
        Status providerName serverName -> 
            withSelectedServerState providerName serverName \state -> 
              do print ("server is " ++ case state of
                   ServerIsDown _   -> "down"
                   ServerIsUp _ -> "up")
        Up providerName serverName ->
            withSelectedServerState providerName serverName \(ServerIsDown action) -> 
              do ips <- startupServer action
                 for_ ips \(IPAddress ip) -> Data.Text.IO.putStrLn ip
        Down providerName serverName ->
            withSelectedServerState providerName serverName \(ServerIsUp action) ->
              do shutdownServer action
  where
    withSelectedProvider :: forall r . ProviderName -> (forall server. (FromJSON server, ToJSON server) => Provider server -> IO r) -> IO r
    withSelectedProvider providerName callback =
      do let Just makeProvider = Data.Map.lookup providerName plugins
         provider <- makeProvider
         withSomeProvider provider callback

    withSelectedServerState :: forall r . ProviderName -> ServerName -> (ServerState -> IO r) -> IO r
    withSelectedServerState providerName serverName callback =
        withSelectedProvider providerName \provider ->  
          do servers <- load
             let Just selectedProviderConfs = Data.Map.lookup providerName servers
                 Just v = Data.Map.lookup serverName selectedProviderConfs
                 Data.Aeson.Success server = fromJSON v
             queriedState <- serverState provider server
             callback queriedState

load :: HasCallStack => IO (Map ProviderName (Map ServerName Data.Aeson.Value))
load = 
  do path <- xdgConfPath
     parseResult <- eitherDecodeFileStrict path
     liftError userError parseResult
  where
    xdgConfPath :: IO FilePath
    xdgConfPath = do
        xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
        let file = xdg </> "config.json" 
        log ("Looking for configuration file " ++ file ++ ".")
        return file

