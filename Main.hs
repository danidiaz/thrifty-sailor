{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO
import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Proxy
import           Data.Monoid
import           Data.Functor.Const
import           Control.Monad.Except
import qualified Data.Text.Read
import qualified Data.Aeson.Encode.Pretty
import           Data.Function ((&))
import           Options.Applicative
import           Control.Lens hiding ((.=))
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text as Text

import GHC.TypeLits (Symbol)
import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.NP
import qualified Generics.SOP.Type.Metadata as M

import ThriftySailor (Token
                     ,droplets
                     ,snapshots
                     ,dropletName
                     ,dropletId
                     ,regionSlug
                     ,dropletStatus
                     ,DropletStatus(..)
                     ,snapshotName
                     ,snapshotId
                     ,snapshotRegionSlugs
                     ,shutdown
                     ,snapshot
                     ,deleteDroplet
                     ,deleteSnapshot
                     ,DropletCreation(..)
                     ,createDroplet)
import ThriftySailor.Prelude

-- many attributes here are similar to the droplet creation request...
data Config = Config 
            { 
                doTokenEnvVar :: String 
            ,   confDropletName :: Text 
            ,   confSnapshotName :: Text
            ,   confRegionSlug :: Text
            ,   confSizeSlug :: Text
            } deriving (Eq,Show,GHC.Generic)

instance Generic Config
instance HasDatatypeInfo Config

type AliasesFor ns = NP (K Text) ns

configAliases :: AliasesFor (FieldNamesOf Config)
configAliases =
      alias @"doTokenEnvVar"     "token_environment_variable"
   :* alias @"confDropletName"   "droplet_name"
   :* alias @"confSnapshotName"  "snapshot_name"
   :* alias @"confRegionSlug"    "region_slug"
   :* alias @"confSizeSlug"      "size_slug"
   :* Nil

alias :: forall (k :: Symbol). Text -> K Text k
alias = K

sample :: Config
sample = Config "DIGITAL_OCEAN_TOKEN"
                "dummy_droplet_name"
                "dummy_snapshot_name"
                "ams3"
                "s-1vcpu-1gb"

recordFromJSON :: forall r xs ns.
                  (IsProductType r xs, 
                   HasDatatypeInfo r,
                   FieldNamesOf r ~ ns,
                   AllZip (LiftedCoercible (K Text) (K Text)) ns xs,
                   Generics.SOP.All FromJSON xs) 
               => AliasesFor ns
               -> Value
               -> Data.Aeson.Types.Parser r
recordFromJSON aliases value = 
    let aliases_xs :: NP (K Text) xs= hcoerce aliases
        parsers :: NP Parser2 xs = cpure_NP (Proxy @FromJSON) 
                                            (Parser2 (\fieldName o -> o .: fieldName))
        Parser1 gp = sequence_NP (liftA2_NP (\(K fieldName) (Parser2 f) -> Parser1 (f fieldName)) 
                                            aliases_xs
                                            parsers)
     in Generics.SOP.to . SOP . Z <$> withObject "Record" gp value

recordToJSON :: forall r xs ns.
                (IsProductType r xs, 
                 HasDatatypeInfo r,
                 FieldNamesOf r ~ ns,
                 AllZip (LiftedCoercible (K Text) (K Text)) ns xs,
                 Generics.SOP.All ToJSON xs) 
             => AliasesFor ns
             -> r
             -> Value
recordToJSON aliases r = 
    let aliases_xs :: NP (K Text) xs= hcoerce aliases
        rep = unZ (unSOP (Generics.SOP.from r))
        pairs = hcliftA2 (Proxy @ToJSON) (\(K a) (I x) -> K (a .= x)) aliases_xs rep
     in object (hcollapse pairs)

-- confgFromJSON :: Value -> Data.Aeson.Types.Parser Config
-- confFromJSON = recordFromJSON configAliases
-- 
-- confToJSON :: Config -> Value
-- confToJSON = recordToJSON configAliases

type family FieldNamesOf r :: [Symbol] where
    FieldNamesOf r = FieldNames (DatatypeInfoOf r)

type family FieldNames (a :: M.DatatypeInfo) :: [Symbol] where
    FieldNames ('M.ADT moduleName datatypeName '[M.Record constructorName fields]) = FieldNames' fields

type family FieldNames' (a :: [M.FieldInfo]) :: [Symbol] where
    FieldNames' '[] = '[]
    FieldNames' (('M.FieldInfo n) ': xs) = n ': FieldNames' xs

newtype Parser1 a = Parser1 { parseJSON1 :: Object -> Data.Aeson.Types.Parser a } deriving Functor

instance Applicative Parser1 where
    pure x = Parser1 (pure (pure x))
    Parser1 pa <*> Parser1 pb = Parser1 $ \v -> pa v <*> pb v 

newtype Parser2 a = Parser2 { parseJSON2 :: Text -> Object -> Data.Aeson.Types.Parser a } 

instance FromJSON Config where
    parseJSON = recordFromJSON configAliases

instance ToJSON Config where
    toJSON = recordToJSON configAliases

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
               let Config {confDropletName,confRegionSlug,confSnapshotName} = conf
               hPutStrLn stderr $ "Looking for droplet with name " 
                               ++ Text.unpack confDropletName
                               ++ " in region "
                               ++ Text.unpack confRegionSlug
               let conditions = [ has $ dropletName.only confDropletName
                                , has $ regionSlug.only confRegionSlug ]
               ds <- droplets token
               d <- unique (\candidate -> alaf All foldMap ($ candidate) conditions)
                           (userError . show)
                           ds
               hPutStrLn stderr $ "Target droplet found."                        
               let status = view dropletStatus d
               hPutStrLn stderr $ "Droplet status is " ++ show status ++ "."                        
               hPutStrLn stderr $ "Checking that snapshot with name " 
                               ++ Text.unpack confSnapshotName
                               ++ " doesn't already exist."
               let snapshotConditions = [ has $ snapshotName.only confSnapshotName
                                        , has $ snapshotRegionSlugs.folded.only confRegionSlug ]
               ss <- snapshots token
               absent (\candidate -> alaf All foldMap ($ candidate) snapshotConditions)
                      (userError . show)
                      ss
               case view dropletStatus d of
                   Active -> 
                        do shutdown token (view dropletId d)
                           pure ()
                   Off -> pure ()
                   _ -> throwError (userError ("Droplet not in valid status for snapshot."))
               hPutStrLn stderr $ "Taking snapshot..." 
               snapshot token (view dropletId d) confSnapshotName 
               hPutStrLn stderr $ "Deleting droplet..." 
               deleteDroplet token (view dropletId d)
               hPutStrLn stderr $ "Done."
        Up -> 
            do (conf,token) <- load
               let Config {confDropletName,confRegionSlug,confSnapshotName,confSizeSlug} = conf
               hPutStrLn stderr $ "Looking for snapshot with name " 
                               ++ Text.unpack confSnapshotName
                               ++ " in region "
                               ++ Text.unpack confRegionSlug
               let snapshotConditions = [ has $ snapshotName.only confSnapshotName
                                        , has $ snapshotRegionSlugs.folded.only confRegionSlug ]
               ss <- snapshots token
               s <- unique (\candidate -> alaf All foldMap ($ candidate) snapshotConditions)
                           (userError . show)
                           ss
               hPutStrLn stderr $ "Target snapshot found."                        
               hPutStrLn stderr $ "Checking that droplet with name " 
                               ++ Text.unpack confDropletName
                               ++ " doesn't exist in region."
                               ++ Text.unpack confRegionSlug
               let conditions = [ has $ dropletName.only confDropletName
                                , has $ regionSlug.only confRegionSlug ]
               ds <- droplets token
               absent (\candidate -> alaf All foldMap ($ candidate) conditions)
                      (userError . show)
                      ds
               hPutStrLn stderr $ "Restoring droplet..."                        
               let Right (snapshotId',_) = Data.Text.Read.decimal (view snapshotId s)
               d <- createDroplet token   
                                  (DropletCreation confDropletName
                                                   confRegionSlug  
                                                   confSizeSlug
                                                   snapshotId')
               hPutStrLn stderr $ "Deleting snapshot..." 
               deleteSnapshot token (view snapshotId s)
               hPutStrLn stderr $ "Done."
    pure ()
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


