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
import qualified Thrifty.Delays
import           Thrifty.Delays (RetryPlan(..),seconds,factor)
import           Thrifty.JSON
import           Thrifty.Network (doGET,doPOST,doDELETE,Token,AbsoluteURL,RelativeURL,extendAbsoluteURL)

data HetznerServer = HetznerServer 
            { 
                _configServerAttrs :: NameAndType
            ,   _configSnapshotName :: Text
            } deriving (Show,Generic,FromRecord,ToRecord)

hetznerServerAliases :: Aliases _
hetznerServerAliases =
     alias @"_configServerAttrs" "server"
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
    ds <- servers token
    let toServer = nameAndType.to (\x -> HetznerServer x (view serverName x <> "_snapshot"))
    pure (toListOf (folded.toServer) ds)
  makeServerState :: HetznerServer -> IO ServerState
  makeServerState (HetznerServer { _configServerAttrs, _configSnapshotName }) = do
    let fallible =
          do log "Checking if target is server n' source is snapshot."
             s <- withExceptT 
                  (:[])
                  (doable 
                      (servers token)
                      (serverMatches  _configServerAttrs)
                      (snapshots token)
                      (snapshotMatches _configSnapshotName))
             return (ServerIsDown (Thrifty.StartupAction do
               log "Restoring server..."                        
               let Right (snapshotId0,_) = Data.Text.Read.decimal (view snapshotId s)
               d <- createServer token _configServerAttrs snapshotId0
               log "Deleting snapshot..." 
               deleteSnapshot token (view snapshotId s)
               log "Echoing server on stdout..." 
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
                      (snapshotMatches _configSnapshotName)
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
               createSnapshot token (view serverId d) 
               log "Deleting server..." 
               deleteServer token (view serverId d)
               log "Done."))
    r <- runExceptT fallible
    case r of
        Left errs -> throwIO (userError ("server in strange state " ++ show errs)) 
        Right actual -> return actual

--
newtype Servers = Servers { getServers :: [Server] } deriving Show

instance FromJSON Servers where
    parseJSON = withObject "Servers" $ \v -> 
        Servers <$> v .: "servers"


-- running, initializing, starting, stopping, off, deleting, migrating, rebuilding, unknown

data Server = Server 
             {
                _serverId :: ServerId
             ,  _status :: ServerStatus
             ,  _nameAndType :: NameAndType
             ,  _networks :: [IF]
             } deriving (Generic,Show)

instance FromJSON Server where
    parseJSON = withObject "Server" \v -> 
      Server <$> v .: "id"
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

type ServerId = Integer

serverId :: Lens' Server Integer
serverId = field' @"_serverId"

serverStatus :: Lens' Server ServerStatus
serverStatus = field' @"_status"

nameAndType :: Lens' Server NameAndType
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

--
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

snapshots :: MonadIO m => Token -> m [Snapshot]
snapshots token = getSnapshots <$> liftIO (doGET' "/v1/images?type=snapshot" token)

newtype Snapshots = Snapshots { getSnapshots :: [Snapshot] }

instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \v -> 
        Snapshots <$> v .: "images"

data Snapshot = Snapshot
              {
                _snapshotId :: SnapshotId
              } deriving (Generic,Show)

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \v -> 
        Snapshot <$> v .: "id"

type SnapshotId = Text

snapshotId :: Lens' Snapshot Text
snapshotId = field' @"_snapshotId"

--
serverMatches :: NameAndType -> Server -> Bool 
serverMatches attrs d =
    attrs == view nameAndType d   

snapshotMatches :: SnapshotId -> Snapshot -> Bool
snapshotMatches snapshotId0 s =
       snapshotId0 == view snapshotId s

--
action :: Token -> ActionId -> IO Action
action token actionId0 = 
    do WrappedAction a <- doGET' (fromString ("/v1/actions/" ++ show actionId0)) token
       return a

--
-- 
createSnapshot :: Token -> ServerId -> IO Action
createSnapshot token serverId0 = 
    do WrappedAction a <- doPOST' 
                          (fromString ("/v1/servers/"++ show serverId0 ++"/actions/create_image"))
                          []
                          (object ["type" .= String "snapshot","description" .= String "thrifty snapshot"])
                          token
       log ("Initiated snapshot action: " ++ show a)
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))

-- Unlike the deletes in DO, this returns a body pointing to an action.
deleteSnapshot :: Token -> SnapshotId -> IO ()
deleteSnapshot token snapshotId0 = 
    do WrappedAction a <- doDELETE' (fromString ("/v1/images/" ++Data.Text.unpack snapshotId0)) token
       log ("Initiated delete snapshot action: " ++ show a)
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))
       pure ()

-- Unlike the deletes in DO, this returns a body pointing to an action.
deleteServer :: Token -> ServerId -> IO ()
deleteServer token serverId0 = 
    do WrappedAction a <- doDELETE' (fromString ("/v1/servers/" ++show serverId0)) token
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))
       pure ()


createServer :: Token -> NameAndType -> SnapshotId -> IO Server
createServer token (NameAndType {_serverName,_serverType}) imageId = 
    do d <- doPOST' 
            ("/v1/servers/") 
            []
            (object 
              [
                 "name" .= _serverName,
                 "server_type" .= _serverType,
                 "image" .= imageId
              ])
            token
       log ("Initiated server creation: " ++ show d)
       complete (serverStatus._Void.united)
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

servers :: MonadIO m => Token -> m [Server]
servers token = getServers <$> liftIO (doGET' "/v1/servers" token)

shutdownServer :: Token -> ServerId -> IO Action
shutdownServer token serverId0 =
    do WrappedAction a <- doPOST' 
                          (fromString ("/v2/servers/"++ show serverId0 ++"/actions/poweroff"))
                          []
                          ()
                          token
       log ("Initiated shutdown action: " ++ show a)
       complete (actionStatus._ActionError)
                (actionStatus._ActionSuccess)
                (action token (view actionId a))

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

doDELETE' :: FromJSON result => RelativeURL -> Token -> IO result
doDELETE' = doDELETE . extendAbsoluteURL baseURL

