{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.Main (defaultMain,ProviderName(..),tokenFromEnvironment) where

import           Prelude hiding (log)

import           System.Directory
import           System.FilePath
import           System.Environment
import           Data.Aeson
import           Data.Foldable (for_)
import           Data.String (IsString(..))
import           Control.Monad.Except
import           Options.Applicative
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text             
import qualified Data.Text.IO             
import           GHC.Generics 
import           Data.Map(Map)
import qualified Data.Map
import qualified Data.ByteString.Lazy
import           System.IO (stdout)

import           Thrifty.Prelude
import           Thrifty.JSON
import           Thrifty

tokenFromEnvironment :: String -> (String -> SomeProvider) -> IO SomeProvider
tokenFromEnvironment variableName makeProvider =
  do token <- getEnv variableName
     return (makeProvider token)

xdgConfPath :: IO FilePath
xdgConfPath = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    log ("Looking for configuration file " ++ file ++ ".")
    return file

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

defaultMain :: [(ProviderName,IO SomeProvider)] -> IO ()
defaultMain (Data.Map.fromList -> plugins) = do
    command <- O.execParser parserInfo
    case command of
        Providers -> 
            do let providerNames = Data.Map.keys plugins
               for_ providerNames \(ProviderName name) -> Data.Text.IO.putStrLn name
        Candidates providerName -> 
            do let Just makeProvider = Data.Map.lookup providerName plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                       do cs <- candidates 
                          Data.ByteString.Lazy.hPut stdout (encode cs)
        Status providerName serverName -> 
            do let Just makeProvider = Data.Map.lookup providerName plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup providerName confs
                            Just v = Data.Map.lookup serverName selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        state <- serverState conf
                        print ("server is " ++ case state of
                            ServerIsDown _   -> "down"
                            ServerIsUp _ -> "up")
        Up providerName serverName ->
            do let Just makeProvider = Data.Map.lookup providerName plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup providerName confs
                            Just v = Data.Map.lookup serverName selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        ServerIsDown action <- serverState conf
                        action
        Down providerName serverName ->
            do let Just makeProvider = Data.Map.lookup providerName plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup providerName confs
                            Just v = Data.Map.lookup serverName selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        ServerIsUp action <- serverState conf
                        action
  where
    load :: IO (Map ProviderName (Map ServerName Data.Aeson.Value))
    load = xdgConfPath >>= Data.Aeson.eitherDecodeFileStrict >>= liftError userError 

