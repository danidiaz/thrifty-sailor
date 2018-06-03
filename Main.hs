{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Directory
import System.FilePath
import System.Environment
import System.IO
import Data.Aeson
import Data.Monoid
import qualified Data.Aeson.Encode.Pretty
import Data.Function ((&))
import Options.Applicative
import Control.Monad.Except
import Control.Lens hiding ((.=))
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text             as Text

import ThriftySailor (Token
                     ,droplets
                     ,snapshots
                     ,dropletName
                     ,dropletId
                     ,regionSlug
                     ,dropletStatus
                     ,DropletStatus(..)
                     ,shutdown)
import ThriftySailor.Prelude

data Config = Config 
            { 
                doTokenEnvVar :: String 
            ,   confDropletName :: Text 
            ,   confSnapshotName :: Text
            ,   confRegionSlug :: Text
            } deriving (Eq,Show)

sample :: Config
sample = Config "DIGITAL_OCEAN_TOKEN"
                "dummy_droplet_name"
                "dummy_snapshot_name"
                "ams3"

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> 
        Config <$> v .: "token_environment_variable"
               <*> v .: "droplet_name"
               <*> v .: "snapshot_name"
               <*> v .: "region_slug"

instance ToJSON Config where
    toJSON (Config {doTokenEnvVar,confDropletName,confSnapshotName,confRegionSlug}) =
          object [ "token_environment_variable" .= doTokenEnvVar
                 , "droplet_name" .= confDropletName
                 , "snapshot_name" .= confSnapshotName
                 , "region_slug" .= confRegionSlug
                 ]

data Command = Example | Status | Up | Down deriving (Eq,Show)

data NameDesc = NameDesc { name :: String, desc :: String }

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = 
            O.subparser 
          . mconcat
          $ [ subcommand 
              (pure Example)  
              (NameDesc "example" 
                        "Print example configuration to stdout")
            , subcommand 
              (pure Status)  
              (NameDesc "status" 
                        "Show current status of the target server")
            , subcommand 
              (pure Up)  
              (NameDesc "up" 
                        "Restores server from snapshot, deletes snapshot")
            , subcommand 
              (pure Down)
              (NameDesc "down" 
                        "Shuts down server, makes snapshot, destroys server")
            ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p (NameDesc {name,desc}) = O.command name (infoHelpDesc p desc)

data Msgs = Msgs 
    {
        lookingForConfFile :: FilePath -> String,
        tokenNotFound :: String -> String
    }

msgs :: Msgs
msgs = 
    Msgs 
    (\file -> "Looking for configuration file " ++ file ++ ".")
    (\var -> "Token " ++ var ++ " not found in environment.") 

defaultMain :: IO ()
defaultMain = defaultMainWith msgs

defaultMainWith :: Msgs -> IO ()
defaultMainWith msgs = do 
    command <- O.execParser parserInfo
    let load = do path <- xdgConfPath
                  conf <- loadConf path
                  loadToken conf
    case command of
        Example -> 
              Data.ByteString.Lazy.Char8.putStrLn 
            . Data.Aeson.Encode.Pretty.encodePretty  
            $ sample 
        Status -> 
            do (conf,token) <- load
               drops <- droplets token
               print drops
               snaps <- snapshots token
               print snaps
        Down -> 
            do (conf,token) <- load
               ds <- droplets token
               let Config {confDropletName,confRegionSlug} = conf
               hPutStrLn stderr $ "Looking for droplet with name " 
                               ++ Text.unpack confDropletName
                               ++ " in region "
                               ++ Text.unpack confRegionSlug
               let conditions = [ has $ dropletName.only confDropletName
                                , has $ regionSlug.only confRegionSlug ]
               d <- unique (\candidate -> alaf All foldMap ($ candidate) conditions)
                           (userError . show)
                           ds
               hPutStrLn stderr $ "Target droplet found."                        
               let status = view dropletStatus d
               hPutStrLn stderr $ "Droplet status is " ++ show status ++ "."                        
               case view dropletStatus d of
                   Active -> 
                        do shutdown token (view dropletId d)
                   Off -> pure ()
                   _ -> throwError (userError ("Droplet not in valid status for snapshot."))
               pure () 
        _ -> 
            do (conf,token) <- load
               print $ conf
               print $ token
    return ()
  where
    xdgConfPath :: IO FilePath
    xdgConfPath = do
        xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
        let file = xdg </> "config.json" 
        hPutStrLn stderr $ lookingForConfFile msgs file
        return file
    loadConf :: FilePath -> IO Config
    loadConf file = do
        e <- eitherDecodeFileStrict' file
        eitherError userError e
    loadToken :: Config -> IO (Config,Token)
    loadToken conf@(Config {doTokenEnvVar}) = do
        token <- do m <- lookupEnv doTokenEnvVar 
                    maybeError (userError (tokenNotFound msgs doTokenEnvVar)) m
        return (conf,token)
    
main :: IO ()
main = defaultMainWith msgs


