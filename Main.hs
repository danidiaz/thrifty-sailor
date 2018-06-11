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
                               ,droplets
                               ,snapshots
                               ,dropletId
                               ,dropletAttrs
                               ,dropletStatus
                               ,DropletStatus(..)
                               ,snapshotName
                               ,snapshotId
                               ,snapshotRegionSlugs
                               ,shutdown
                               ,snapshot
                               ,deleteDroplet
                               ,deleteSnapshot
                               ,NameRegionSize(..)
                               ,name
                               ,regionSlug
                               ,sizeSlug
                               ,createDroplet)
import           ThriftySailor.JSON


-- many attributes here are similar to the droplet creation request...
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

data Command = Example | Status | UpDown UpDown deriving (Eq,Show)

data UpDown = Up | Down deriving (Eq,Show)

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
            , subcommand (pure (UpDown Up))
                         (NameDesc "up" 
                                   "Restores server from snapshot, deletes snapshot")
            , subcommand (pure (UpDown Down))
                         (NameDesc "down" 
                                   "Shuts down server, makes snapshot, destroys server") ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p (NameDesc {optionName,optionDesc}) = 
        O.command optionName (infoHelpDesc p optionDesc)

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
        UpDown upOrDown ->
            do (Config {configDropletAttrs,configSnapshotName},token) <- load
               let NameRegionSize {_name,_regionSlug,_sizeSlug} = configDropletAttrs
                   dropletMatches attrs d = 
                       attrs == view dropletAttrs d
                   snapshotMatches attrs s =
                          view snapshotName s == view name attrs 
                       && any (== (view regionSlug attrs)) (view snapshotRegionSlugs s) 
               case upOrDown of
                   Down -> 
                       do log ("Looking for droplet that matches " ++ show configDropletAttrs)
                          ds <- droplets token
                          d <- unique (dropletMatches configDropletAttrs)
                                      (userError . show)
                                      ds
                          log "Target droplet found."                        
                          let status = view dropletStatus d
                          log ("Checking that no snapshot matches " ++ show configDropletAttrs) 
                          ss <- snapshots token
                          absent (snapshotMatches configDropletAttrs)
                                 (userError . show)
                                 ss
                          log ("Droplet status is " ++ show status ++ ".")
                          case view dropletStatus d of
                              Active -> 
                                   do shutdown token (view dropletId d)
                                      pure ()
                              Off -> pure ()
                              _ -> throwError (userError ("Droplet not in valid status for snapshot."))
                          log "Taking snapshot..." 
                          snapshot token (view dropletId d) configSnapshotName 
                          log "Deleting droplet..." 
                          deleteDroplet token (view dropletId d)
                          log "Done."
                   Up -> 
                       do log ("Looking for snapshot compatible with" ++ show configDropletAttrs)
                          ss <- snapshots token
                          s <- unique (snapshotMatches configDropletAttrs)
                                      (userError . show)
                                      ss
                          log "Target snapshot found."                        
                          log ("Checking that no droplet matches " ++ show configDropletAttrs) 
                          ds <- droplets token
                          absent (dropletMatches configDropletAttrs)
                                 (userError . show)
                                 ds
                          log "Restoring droplet..."                        
                          let Right (isnapshotId,_) = Data.Text.Read.decimal (view snapshotId s)
                          createDroplet token   
                                        configDropletAttrs
                                        isnapshotId
                          log "Deleting snapshot..." 
                          deleteSnapshot token (view snapshotId s)
                          log "Done."
    pure ()

