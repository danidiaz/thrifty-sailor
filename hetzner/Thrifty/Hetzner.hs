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
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Lens hiding ((.=))
import           Control.Exception
import           Data.Traversable
import           Data.Monoid
import           Data.Text (Text)            
import qualified Data.Text
import qualified Data.Text.Read
import qualified Data.Map
import           Data.Map (Map)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.String (fromString)
import           Data.Generics.Product.Fields (field')
import           Data.Generics.Sum.Constructors (_Ctor')
import           GHC.Generics (Generic)
import qualified Data.RBR
import           Data.RBR (FromRecord,ToRecord,unit)
import           Data.Aeson
import           Data.Scientific (floatingOrInteger)

import           Thrifty
import           Thrifty.Prelude
import qualified Thrifty.Delays
import           Thrifty.Delays (RetryPlan(..),seconds,factor)
import           Thrifty.JSON
import           Thrifty.Network (doGET,doPOST,doDELETE,doDELETE_,Token,AbsoluteURL,RelativeURL,extendAbsoluteURL)

data HetznerServer = HetznerServer 
            { 
                _configServerAttrs :: PersistentAttributes
            ,   _configSnapshotLabelValue :: SnapshotLabelValue
            } deriving (Show,Generic,FromRecord,ToRecord)

hetznerServerAliases :: Aliases _
hetznerServerAliases =
     alias @"_configServerAttrs" "server"
   . alias @"_configSnapshotLabelValue" "snapshot_label_value"
   $ unit

instance FromJSON HetznerServer where
    parseJSON = nominalRecordFromJSON hetznerServerAliases

instance ToJSON HetznerServer where
    toJSON = recordToJSON hetznerServerAliases

makeHetzner :: Token -> Provider HetznerServer  
makeHetzner token = Provider makeCandidates makeServerState
  where
  makeCandidates :: IO [HetznerServer]
  makeCandidates = do
    ds <- servers token
    let toServer = attributes.to (\x -> HetznerServer x (view serverName x <> "_snapshot"))
    pure (toListOf (folded.toServer) ds)
  makeServerState :: HetznerServer -> IO ServerState
  makeServerState (HetznerServer { _configServerAttrs, _configSnapshotLabelValue }) = do
    let fallible =
          do log "Checking if target is server n' source is snapshot."
             s <- withExceptT 
                  (:[])
                  (doable 
                      (servers token)
                      (serverMatches  _configServerAttrs)
                      (snapshots token)
                      (snapshotMatches _configSnapshotLabelValue))
             return (ServerIsDown (Thrifty.StartupAction do
               log "Restoring server..."                        
               d <- createServer token _configServerAttrs (view snapshotId s)
               log "Deleting snapshot..." 
               deleteSnapshot token (view snapshotId s)
               log "Echoing server ip on stdout..." 
               case toListOf (networks.folded.address) d of
                 [] -> 
                    throwError (userError "No public ip on server!")
                 ip : [] -> 
                    return (IPAddress ip :| [])
                 ip : ips  -> 
                    do log "More than one public ip on server!"
                       return (IPAddress ip :| (IPAddress <$> ips))))
          <|>
          do log "Checking if target is snapshot n' source is server."
             d <- withExceptT
                  (:[])
                  (doable
                      (snapshots token)
                      (snapshotMatches _configSnapshotLabelValue)
                      (servers token)
                      (serverMatches _configServerAttrs))
             return (ServerIsUp (Thrifty.ShutdownAction do
               log ("Server status is " ++ show (view serverStatus d) ++ ".")
               case view serverStatus d of
                   Running -> do Thrifty.Hetzner.shutdownServer token (view serverId d)
                                 pure ()
                   Off -> pure ()
                   _ -> throwError (userError ("Server not in valid status for snapshot."))
               log "Taking snapshot..." 
               createSnapshot token _configSnapshotLabelValue (view serverId d) 
               log "Deleting server..." 
               deleteServer token (view serverId d)
               log "Done."))
    r <- runExceptT fallible
    case r of
        Left errs -> throwIO (userError ("server in strange state " ++ show errs)) 
        Right actual -> return actual

--
--
servers :: MonadIO m => Token -> m [Server]
servers token = getServers <$> liftIO (doGET' "/v1/servers" token)

newtype Servers = Servers { getServers :: [Server] } deriving Show

instance FromJSON Servers where
    parseJSON = withObject "Servers" $ \v -> 
        Servers <$> v .: "servers"

serverMatches :: PersistentAttributes -> Server -> Bool 
serverMatches attrs d =
    attrs == view attributes d   

createServer :: Token -> PersistentAttributes -> SnapshotId -> IO Server
createServer token (PersistentAttributes {_serverName,_serverType,_serverLocation}) imageId = 
    do WrappedServer d <- doPOST' 
            ("/v1/servers/") 
            []
            (object 
              [
                 "name" .= _serverName,
                 "server_type" .= _serverType,
                 "location" .= _serverLocation,
                 "image" .= show imageId
              ])
            token
       log ("Initiated server creation: " ++ show d)
       complete (serverStatus._Void.united) -- no explicit error state, we don't match anything
                (serverStatus._Running)
                (server token (view serverId d))

server :: MonadIO m => Token -> ServerId -> m Server
server token serverId0 = 
    do WrappedServer d <- liftIO (doGET' (fromString ("/v1/servers/" ++ show serverId0)) token)
       return d

newtype WrappedServer = WrappedServer { getServer :: Server } deriving Show

instance FromJSON WrappedServer where
    parseJSON = withObject "WrappedServer" $ \v -> 
        WrappedServer <$> v .: "server"

-- running, initializing, starting, stopping, off, deleting, migrating, rebuilding, unknown
data Server = Server 
             {
                _serverId :: ServerId
             ,  _status :: ServerStatus
             ,  _networks :: [IF]
             ,  _attributes :: PersistentAttributes
             } deriving (Generic,Show)

instance FromJSON Server where
    parseJSON = withObject "Server" \v -> 
      Server <$> v .: "id"
             <*> v .: "status"
             <*> (do publicNetworks <- v .: "public_net"
                     publicIPv4Networks <- publicNetworks .: "ipv4"
                     ip <- publicIPv4Networks .: "ip"
                     return [IF ip]
                  <|>
                  return [])
             <*> do name <- 
                        v .: "name"
                    typeName <- 
                        do serverType <- v .: "server_type"
                           serverType .: "name"
                    locationName <-
                        do datacenter <- v .: "datacenter"
                           location <- datacenter .: "location"
                           location .: "name"
                    return (PersistentAttributes name typeName locationName)

type ServerId = Integer

serverId :: Lens' Server Integer
serverId = field' @"_serverId"

serverStatus :: Lens' Server ServerStatus
serverStatus = field' @"_status"

attributes :: Lens' Server PersistentAttributes
attributes = field' @"_attributes"

data PersistentAttributes = PersistentAttributes
    {
        _serverName :: Text,
        _serverType :: Text,
        _serverLocation :: Text
    } deriving (Generic,Show,Eq,ToRecord,FromRecord)

persistentAttributesAliases :: Aliases _
persistentAttributesAliases =
     alias @"_serverName" "server_name"
   . alias @"_serverType" "server_type"
   . alias @"_serverLocation" "server_location"
   $ unit

-- | Used only in Config object.
instance FromJSON PersistentAttributes where
    parseJSON = nominalRecordFromJSON persistentAttributesAliases

-- | Used only in Config object.
instance ToJSON PersistentAttributes where
    toJSON = recordToJSON persistentAttributesAliases

serverName :: Lens' PersistentAttributes Text
serverName = field' @"_serverName"

serverType :: Lens' PersistentAttributes Text
serverType = field' @"_serverType"

serverLocation :: Lens' PersistentAttributes Text
serverLocation = field' @"_serverLocation"

networks :: Lens' Server [IF]
networks = field' @"_networks"

data ServerStatus = Running | Initializing | Starting | Stopping | Off | Deleting | Migrating | Rebuilding | Unknown deriving (Show,Eq,Generic)

instance FromJSON ServerStatus where
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

_Running :: Traversal' ServerStatus ()
_Running = _Ctor' @"Running"



-- Unlike the deletes in DO, this returns a body pointing to an action.
deleteServer :: Token -> ServerId -> IO ()
deleteServer token serverId0 = 
    do WrappedAction a <- doDELETE' (fromString ("/v1/servers/" ++show serverId0)) token
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))
       pure ()

shutdownServer :: Token -> ServerId -> IO Action
shutdownServer token serverId0 =
    do WrappedAction a <- doPOST' 
                          (fromString ("/v1/servers/"++ show serverId0 ++"/actions/poweroff"))
                          []
                          (object [])
                          token
       log ("Initiated shutdown action: " ++ show a)
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))

--

snapshots :: MonadIO m => Token -> m [Snapshot]
snapshots token = getSnapshots <$> liftIO (doGET' "/v1/images?type=snapshot" token)

newtype Snapshots = Snapshots { getSnapshots :: [Snapshot] }

instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \v -> 
        Snapshots <$> v .: "images"

data Snapshot = Snapshot
              {
                _snapshotId :: SnapshotId,
                _snapshotLabels :: Map Text SnapshotLabelValue
              } deriving (Generic,Show)

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \v -> 
        do Right i <- floatingOrInteger <$> v .: "id" 
           labels <- v .: "labels"
           pure (Snapshot i labels)

type SnapshotId = Int

thriftyKey :: Text
thriftyKey = "thrifty-stable-snapshot-identifier"

type SnapshotLabelValue = Text

snapshotId :: Lens' Snapshot SnapshotId
snapshotId = field' @"_snapshotId"

snapshotLabels :: Lens' Snapshot (Map Text SnapshotLabelValue)
snapshotLabels = field' @"_snapshotLabels"

snapshotMatches :: SnapshotLabelValue -> Snapshot -> Bool
snapshotMatches snapshotLabelValue0 =
    has (snapshotLabels.ix thriftyKey.only snapshotLabelValue0)

createSnapshot :: Token -> SnapshotLabelValue -> ServerId -> IO Action
createSnapshot token label serverId0 = 
    do WrappedAction a <- doPOST' 
                          (fromString ("/v1/servers/"++ show serverId0 ++"/actions/create_image"))
                          []
                          (object 
                            [   
                                "type" .= String "snapshot",
                                "description" .= String "thrifty snapshot",
                                "labels" .= 
                                    object [ thriftyKey .= label ]
                            ])
                          token
       log ("Initiated snapshot action: " ++ show a)
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))

-- According to the docs, this returns immediately without a body (unlike
-- deleting a server)
deleteSnapshot :: Token -> SnapshotId -> IO ()
deleteSnapshot token snapshotId0 = 
    do doDELETE_' (fromString ("/v1/images/" ++show snapshotId0)) token
       pure ()

--
--
action :: Token -> ActionId -> IO Action
action token actionId0 = 
    do WrappedAction a <- doGET' (fromString ("/v1/actions/" ++ show actionId0)) token
       return a

newtype WrappedAction = WrappedAction { getAction :: Action } deriving Show

instance FromJSON WrappedAction where
    parseJSON = withObject "WrappedAction" $ \v -> 
        WrappedAction <$> v .: "action"

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
           giveUpAfter = seconds 1800,
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

doDELETE' :: FromJSON result => RelativeURL -> Token -> IO result
doDELETE' = doDELETE . extendAbsoluteURL baseURL

doDELETE_' :: RelativeURL -> Token -> IO ()
doDELETE_' = doDELETE_ . extendAbsoluteURL baseURL
