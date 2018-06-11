{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Prelude hiding (log)
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO
import           Data.Aeson
import           Data.Monoid
import           Control.Monad.Except
import qualified Data.Text.Read
import qualified Data.Aeson.Encode.Pretty
import           Options.Applicative
import           Control.Lens 
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text as Text

import qualified GHC.Generics as GHC
import           Generics.SOP

import           ThriftySailor.Prelude
import           ThriftySailor (Token
                               ,Droplet
                               ,droplets
                               ,dropletId
                               ,dropletAttrs
                               ,dropletStatus
                               ,DropletStatus(..)
                               ,shutdownDroplet
                               ,deleteDroplet
                               ,Snapshot
                               ,snapshots
                               ,SnapshotName
                               ,snapshotName
                               ,snapshotId
                               ,snapshotRegionSlugs
                               ,createSnapshot
                               ,deleteSnapshot
                               ,NameRegionSize(..)
                               ,name
                               ,regionSlug
                               ,createDroplet)
import           ThriftySailor.JSON


data Config = Config 
            { 
                doTokenEnvVar :: String 
            ,   configDropletAttrs :: NameRegionSize
            ,   configSnapshotName :: Text
            } deriving (Show,GHC.Generic)

instance Generic Config
instance HasDatatypeInfo Config

configAliases :: AliasesFor (FieldNamesOf Config)
configAliases =
      alias @"doTokenEnvVar"      "token_environment_variable"
   :* alias @"configDropletAttrs" "droplet"
   :* alias @"configSnapshotName" "snapshot_name"
   :* Nil

instance FromJSON Config where
    parseJSON = recordFromJSON configAliases

instance ToJSON Config where
    toJSON = recordToJSON configAliases

sample :: Config
sample = Config "DIGITAL_OCEAN_TOKEN"
                (NameRegionSize "dummy_droplet_name"
                                "ams3"
                                "s-1vcpu-1gb")
                "dummy_snapshot_name"

xdgConfPath :: IO FilePath
xdgConfPath = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    log ("Looking for configuration file " ++ file ++ ".")
    return file

loadConf :: FilePath -> IO Config
loadConf file = do
    e <- eitherDecodeFileStrict' file
    eitherError userError e

loadToken :: Config -> IO (Config,Token)
loadToken conf@(Config {doTokenEnvVar}) = do
    token <- do m <- lookupEnv doTokenEnvVar 
                let message = "Token " ++ doTokenEnvVar ++ " not found in environment." 
                maybeError (userError message) m
    return (conf,token)

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
    subcommand p (NameDesc {optionName,optionDesc}) = 
        O.command optionName (infoHelpDesc p optionDesc)

doable :: (Show target, Show source)
       => IO [target]
       -> (target -> Bool)
       -> IO [source]
       -> (source -> Bool)
       -> IO source
doable listTargets checkTarget listSources checkSource =
    do log "Checking that target doesn't already exist..."
       ts <- listTargets
       absent checkTarget (userError . show) ts
       log "Checking that the source exists..."
       ss <- listSources
       s <- unique checkSource (userError . show) ss
       pure s

dropletMatches :: NameRegionSize -> Droplet -> Bool 
dropletMatches attrs d =
    attrs == view dropletAttrs d   

snapshotMatches :: SnapshotName -> Text -> Snapshot -> Bool
snapshotMatches snapshotName0 regionSlug0 s =
       snapshotName0 == view snapshotName s
    && any (== regionSlug0) (view snapshotRegionSlugs s) 

moveDown :: Token -> NameRegionSize -> SnapshotName -> IO ()
moveDown token attrs snapshotName0 =
    do log "Target is snapshot, source is droplet."
       d <- doable (snapshots token)
                   (snapshotMatches snapshotName0 (view regionSlug attrs))
                   (droplets token)
                   (dropletMatches attrs)
       log ("Droplet status is " ++ show (view dropletStatus d) ++ ".")
       case view dropletStatus d of
           Active -> do shutdownDroplet token (view dropletId d)
                        pure ()
           Off ->    pure ()
           _ ->      throwError (userError ("Droplet not in valid status for snapshot."))
       log "Taking snapshot..." 
       createSnapshot token snapshotName0 (view dropletId d) 
       log "Deleting droplet..." 
       deleteDroplet token (view dropletId d)
       log "Done."

moveUp :: Token -> SnapshotName -> NameRegionSize -> IO ()
moveUp token snapshotName0 attrs  =
    do log "Target is droplet, source is snapshot."
       s <- doable (droplets token)
                   (dropletMatches attrs)
                   (snapshots token)
                   (snapshotMatches snapshotName0 (view regionSlug attrs))
       log "Restoring droplet..."                        
       let Right (snapshotId0,_) = Data.Text.Read.decimal (view snapshotId s)
       createDroplet token attrs snapshotId0
       log "Deleting snapshot..." 
       deleteSnapshot token (view snapshotId s)
       log "Done."

main :: IO ()
main = do
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
        Up ->
            do (conf,token) <- load
               moveUp token (configSnapshotName conf) (configDropletAttrs conf)
        Down ->
            do (conf,token) <- load
               moveDown token (configDropletAttrs conf) (configSnapshotName conf)
