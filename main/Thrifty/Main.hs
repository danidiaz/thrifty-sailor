{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.Main (defaultMain) where

import           Prelude hiding (log)

import           System.Directory
import           System.FilePath
import           System.Environment
import           Data.Aeson
import           Control.Monad.Except
import qualified Data.Aeson.Encode.Pretty
import           Options.Applicative
import           Control.Lens 
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text             
import           GHC.Generics 
import           Data.RBR

import           Thrifty.Prelude
import           Thrifty.JSON
import           Thrifty.DO 
                         (
                             droplets
                         ,   snapshots
                         ,   moveUp
                         ,   moveDown
                         ,   NameRegionSize(..)
                         ,   RegionSlug(..)
                         )

doTokenVar :: String 
doTokenVar = "DIGITALOCEAN_ACCESS_TOKEN" 

data Config = Config 
            { 
                _configDropletAttrs :: NameRegionSize
            ,   _configSnapshotName :: Text
            } deriving (Show,Generic)

instance FromRecord Config
instance ToRecord Config

configAliases :: Aliases _
configAliases =
     alias @"_configDropletAttrs" "droplet"
   . alias @"_configSnapshotName" "snapshot_name"
   $ unit

instance FromJSON Config where
    parseJSON = recordFromJSON configAliases

instance ToJSON Config where
    toJSON = recordToJSON configAliases

sample :: Config
sample = Config (NameRegionSize "dummy-droplet-name"
                                (RegionSlug "ams3")
                                "s-1vcpu-1gb")
                "dummy-snapshot-name"

xdgConfPath :: IO FilePath
xdgConfPath = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    log ("Looking for configuration file " ++ file ++ ".")
    return file

data Command = Example | Status | Up | Down deriving (Eq,Show)

data NameDesc = NameDesc { optionName :: String, optionDesc :: String }

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = 
            O.subparser 
          . mconcat
          $ [ subcommand (pure Example)  
                         (NameDesc "example" 
                                   "Print example configuration to stdout")
            , subcommand (pure Status)  
                         (NameDesc "status" 
                                   "Show current status of the target server")
            , subcommand (pure Up)
                         (NameDesc "up" 
                                   "Restores server from snapshot, deletes snapshot")
            , subcommand (pure Down)
                         (NameDesc "down" 
                                   "Shuts down server, makes snapshot, destroys server") ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p nd = 
        O.command (optionName nd) (infoHelpDesc p (optionDesc nd))

defaultMain :: IO ()
defaultMain = do
    command <- O.execParser parserInfo
    case command of
        Example -> 
              Data.ByteString.Lazy.Char8.putStrLn 
            . Data.Aeson.Encode.Pretty.encodePretty  
            $ sample 
        Status -> 
            do token <- getEnv doTokenVar
               drops <- droplets token
               print drops
               snaps <- snapshots token
               print snaps
        Up ->
            do token <- getEnv doTokenVar
               conf <- load
               moveUp token (_configSnapshotName conf) (_configDropletAttrs conf)
        Down ->
            do token <- getEnv doTokenVar
               conf <- load
               moveDown token (_configDropletAttrs conf) (_configSnapshotName conf)
  where
    load :: IO Config
    load = xdgConfPath >>= eitherDecodeFileStrict >>= liftError userError 

