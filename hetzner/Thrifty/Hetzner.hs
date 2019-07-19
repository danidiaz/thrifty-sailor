-- | https://docs.hetzner.cloud/
--   https://github.com/hetznercloud/cli
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.Hetzner (
        Token,
        makeHetzner
    ) where

import           Prelude hiding (log)
import           Data.Foldable
import           Data.Traversable
import           Control.Monad.Except
import           Data.Aeson
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Lens hiding ((.=))
import           Data.Text (Text)            
import qualified Data.Text
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.String (fromString)
import           Control.Exception
import           Data.Generics.Product.Fields (field')
import           Data.Generics.Sum.Constructors (_Ctor')
import           GHC.Generics (Generic)
import           Data.RBR

import qualified Data.Text.Read

import           Thrifty
import           Thrifty.Prelude
import           Thrifty.Delays
import           Thrifty.JSON
import           Thrifty.Network (doGET,doPOST,doDELETE,Token,AbsoluteURL,RelativeURL,extendAbsoluteURL)

data HetznerServer = HetznerServer 
            { 
                _configDropletAttrs :: NameAndType
            ,   _configSnapshotName :: Text
            } deriving (Show,Generic,FromRecord,ToRecord)

hetznerServerAliases :: Aliases _
hetznerServerAliases =
     alias @"_configDropletAttrs" "server"
   . alias @"_configSnapshotName" "snapshot_name"
   $ unit

instance FromJSON HetznerServer where
    parseJSON = nominalRecordFromJSON hetznerServerAliases

instance ToJSON HetznerServer where
    toJSON = recordToJSON hetznerServerAliases

makeHetzner :: Token -> Provider HetznerServer  
makeHetzner token = Provider makeCandidates undefined
  where
  makeCandidates :: IO [HetznerServer]
  makeCandidates = do
    ds <- droplets token
    let toServer = nameAndType.to (\x -> HetznerServer x (view serverName x <> "_snapshot"))
    pure (toListOf (folded.toServer) ds)

--
newtype Droplets = Droplets { getDroplets :: [Droplet] } deriving Show

instance FromJSON Droplets where
    parseJSON = withObject "Droplets" $ \v -> 
        Droplets <$> v .: "droplets"

type DropletId = Integer

droplets :: MonadIO m => Token -> m [Droplet]
droplets token = getDroplets <$> liftIO (doGET' "/v2/droplets" token)

-- running, initializing, starting, stopping, off, deleting, migrating, rebuilding, unknown

data Droplet = Droplet 
             {
                _dropletId :: DropletId
             ,  _status :: DropletStatus
             ,  _nameAndType :: NameAndType
             ,  _networks :: [IF]
             } deriving (Generic,Show)

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" \v -> 
      Droplet <$> v .: "id"
              <*> v .: "status"
              <*> do name <- v .: "name"
                     serverType <- v .: "server_type"
                     serverTypeName <- serverType .: "name"
                     return (NameAndType name serverTypeName)
              <*> (do publicNetworks <- v .: "public_net"
                      publicIPv4Networks <- publicNetworks .: "ipv4"
                      ip <- publicIPv4Networks .: "ip"
                      return [IF ip]
                   <|>
                   return [])

dropletId :: Lens' Droplet Integer
dropletId f s = _dropletId s & f <&> \a -> s { _dropletId = a }

dropletStatus :: Lens' Droplet DropletStatus
dropletStatus = field' @"_status"

nameAndType :: Lens' Droplet NameAndType
nameAndType = field' @"_nameAndType"

data NameAndType = NameAndType
    {
        _serverName :: Text,
        _serverType :: Text
    } deriving (Generic,Show,Eq,ToRecord,FromRecord)

nameAndTypeAliases :: Aliases _
nameAndTypeAliases =
     alias @"_serverName" "server_name"
   . alias @"_serverType" "server_type"
   $ unit

-- | Used only in Config object.
instance FromJSON NameAndType where
    parseJSON = nominalRecordFromJSON nameAndTypeAliases

-- | Used only in Config object.
instance ToJSON NameAndType where
    toJSON = recordToJSON nameAndTypeAliases

serverName :: Lens' NameAndType Text
serverName = field' @"_serverName"

serverType :: Lens' NameAndType Text
serverType = field' @"_serverType"

networks :: Lens' Droplet [IF]
networks = field' @"_networks"

data DropletStatus = Running | Initializing | Starting | Stopping | Off | Deleting | Migrating | Rebuilding | Unknown deriving (Show,Eq,Generic)

instance FromJSON DropletStatus where
    parseJSON = withText "Status" $ \v -> 
        case v of
            "running" -> 
                pure Running
            "initializing" -> 
                pure Initializing
            "starting" -> 
                pure Starting
            "stopping" -> 
                pure Stopping
            "off" -> 
                pure Off
            "deleting" -> 
                pure Deleting
            "migrating" -> 
                pure Migrating
            "rebuilding" -> 
                pure Rebuilding
            "unknown" -> 
                pure Unknown
            _ -> empty

data IF = IF
        {
             _address :: Text
        } deriving (Generic,Show)

address :: Lens' IF Text
address = field' @"_address"


--

data Action = Action
            {
                _actionId :: ActionId
            ,   _actionStatus :: ActionStatus 
            } deriving (Generic,Show)

instance FromJSON Action where
    parseJSON = withObject "Action" $ \v -> 
        Action <$> v .: "id"
               <*> v .: "status"

actionId :: Lens' Action ActionId
actionId = field' @"_actionId"

actionStatus :: Lens' Action ActionStatus
actionStatus = field' @"_actionStatus"

type ActionId = Integer

data ActionStatus = ActionRunning
                  | ActionSuccess
                  | ActionError
                  deriving (Show,Generic)
                   
instance FromJSON ActionStatus where
    parseJSON = withText "ActionStatus" $ \t -> 
        case t of
            "running" -> pure ActionRunning
            "success" -> pure ActionSuccess
            "error" -> pure ActionError
            _ -> empty

_ActionRunning :: Traversal' ActionStatus ()
_ActionRunning = _Ctor' @"ActionRunning" 

_ActionSuccess :: Traversal' ActionStatus ()
_ActionSuccess =  _Ctor' @"ActionSuccess" 

_ActionError :: Traversal' ActionStatus ()
_ActionError = _Ctor' @"ActionError" 

--
complete 
    :: Show a 
    => (Fold a ()) -- ^ error check
    -> (Fold a ()) -- ^ completion check
    -> IO a 
    -> IO a
complete errCheck doneCheck = 
    Thrifty.Delays.complete
    (RetryPlan 
       { 
           giveUpAfter = seconds 360,
           initialDelay = seconds 2,
           increaseFactor = factor 1.5,
           maximumDelay = seconds 15 
       })
    (has errCheck)
    (has doneCheck)
--

baseURL :: AbsoluteURL
baseURL = fromString "https://api.hetzner.cloud"

doGET' :: FromJSON a => RelativeURL -> Token -> IO a 
doGET' = doGET . extendAbsoluteURL baseURL

doPOST' :: (ToJSON body, FromJSON result) => RelativeURL -> [(Text,[Text])] -> body -> Token -> IO result
doPOST' = doPOST . extendAbsoluteURL baseURL 

doDELETE' :: RelativeURL -> Token -> IO ()
doDELETE' = doDELETE . extendAbsoluteURL baseURL

