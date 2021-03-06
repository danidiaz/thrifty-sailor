-- | https://developers.digitalocean.com/documentation/v2/
--   https://github.com/digitalocean/doctl
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.DO (
        Token
    ,   droplets

    ,   Droplet
    ,   dropletId
    ,   dropletAttrs
    ,   dropletStatus
    ,   networks

    ,   IF
    ,   address
    ,   addressType
    ,   IPAddressType(..)
    ,   _PublicIP 

    ,   DropletStatus(..)
    ,   _New
    ,   _Active
    ,   _Off
    ,   _Archive 

    ,   shutdownDroplet
    ,   createSnapshot
    ,   deleteDroplet
    ,   ImageId

    ,   PersistentAttributes(..)
    ,   name
    ,   regionSlug
    ,   sizeSlug

    ,   RegionSlug(..)

    ,   createDroplet

    ,   snapshots

    ,   Snapshot
    ,   SnapshotName
    ,   snapshotId
    ,   snapshotName
    ,   snapshotRegionSlugs

    ,   deleteSnapshot
    ,   doable
    ,   DOServer(..)
    ,   makeDO
    ) where

import           Prelude hiding (log)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Except
import           Control.Lens hiding ((.=))
import           Control.Exception
import           Data.Traversable
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.String (fromString)
import           Data.Text (Text)            
import qualified Data.Text
import qualified Data.Text.Read
import           Data.Generics.Product.Fields (field')
import           Data.Generics.Sum.Constructors (_Ctor')
import           Data.RBR
import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Monoid

import           Thrifty
import           Thrifty.Prelude
import qualified Thrifty.Delays
import           Thrifty.Delays (RetryPlan(..),seconds,factor)
import           Thrifty.JSON
import           Thrifty.Network (doGET,doPOST,doDELETE_,Token,AbsoluteURL,RelativeURL,extendAbsoluteURL)

data DOServer = DOServer 
            { 
                _configDropletAttrs :: PersistentAttributes
            ,   _configSnapshotName :: Text
            } deriving (Show,Generic,FromRecord,ToRecord)

doServerAliases :: Aliases _
doServerAliases =
     alias @"_configDropletAttrs" "droplet"
   . alias @"_configSnapshotName" "snapshot_name"
   $ unit

instance FromJSON DOServer where
    parseJSON = nominalRecordFromJSON doServerAliases

instance ToJSON DOServer where
    toJSON = nominalRecordToJSON doServerAliases

makeDO :: Token -> Provider DOServer  
makeDO token = Provider makeCandidates makeServerState
  where
  makeCandidates :: IO [DOServer]
  makeCandidates = do
    ds <- droplets token
    let toServer = dropletAttrs.to (\x -> DOServer x (view name x <> "_snapshot"))
    pure (toListOf (folded.toServer) ds)
  makeServerState :: DOServer -> IO ServerState
  makeServerState (DOServer { _configDropletAttrs, _configSnapshotName }) = do
    let fallible =
          do log "Checking if target is droplet n' source is snapshot."
             s <- withExceptT 
                  (:[])
                  (doable 
                      (droplets token)
                      (dropletMatches  _configDropletAttrs)
                      (snapshots token)
                      (snapshotMatches _configSnapshotName (view regionSlug _configDropletAttrs)))
             return (ServerIsDown (Thrifty.StartupAction do
               log "Restoring droplet..."                        
               let Right (snapshotId0,_) = Data.Text.Read.decimal (view snapshotId s)
               d <- createDroplet token _configDropletAttrs snapshotId0
               log "Deleting snapshot..." 
               deleteSnapshot token (view snapshotId s)
               log "Echoing droplet on stdout..." 
               case toListOf (networks.folded.filtered (has (addressType._PublicIP)).address) d of
                 [] -> 
                    throwError (userError "No public ip on droplet!")
                 ip : [] -> 
                    return (IPAddress ip :| [])
                 ip : ips  -> 
                    do log "More than one public ip on droplet!"
                       return (IPAddress ip :| (IPAddress <$> ips))))
          <|>
          do log "Checking if target is snapshot n' source is droplet."
             d <- withExceptT
                  (:[])
                  (doable
                      (snapshots token)
                      (snapshotMatches _configSnapshotName (view regionSlug _configDropletAttrs))
                      (droplets token)
                      (dropletMatches _configDropletAttrs))
             return (ServerIsUp (Thrifty.ShutdownAction do
               log ("Droplet status is " ++ show (view dropletStatus d) ++ ".")
               case view dropletStatus d of
                   Active -> do shutdownDroplet token (view dropletId d)
                                pure ()
                   Off ->    pure ()
                   _ ->      throwError (userError ("Droplet not in valid status for snapshot."))
               log "Taking snapshot..." 
               createSnapshot token _configSnapshotName (view dropletId d) 
               log "Deleting droplet..." 
               deleteDroplet token (view dropletId d)
               log "Done."))
    r <- runExceptT fallible
    case r of
        Left errs -> throwIO (userError ("server in strange state " ++ show errs)) 
        Right actual -> return actual

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html
-- | https://developers.digitalocean.com/documentation/v2/
-- | https://developers.digitalocean.com/documentation/v2/#list-all-droplets

newtype Droplets = Droplets { getDroplets :: [Droplet] } deriving Show

instance FromJSON Droplets where
    parseJSON = withObject "Droplets" $ \v -> 
        Droplets <$> v .: "droplets"

type DropletId = Integer

data Droplet = Droplet 
             {
                _dropletId :: DropletId
             ,  _dropletAttrs :: PersistentAttributes
             ,  _status :: DropletStatus
             ,  _networks :: [IF]
             } deriving (Generic,Show)

dropletId :: Lens' Droplet Integer
dropletId f s = _dropletId s & f <&> \a -> s { _dropletId = a }

dropletAttrs :: Lens' Droplet PersistentAttributes
dropletAttrs f s = _dropletAttrs s & f <&> \a -> s { _dropletAttrs = a }

dropletStatus :: Lens' Droplet DropletStatus
dropletStatus = field' @"_status"

networks :: Lens' Droplet [IF]
networks f s = _networks s & f <&> \a -> s { _networks = a }

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" $ \v -> 
        let attrs = PersistentAttributes <$> v .: "name"
                                   <*> do region <- v .: "region"
                                          region .: "slug"
                                   <*> v .: "size_slug"
         in Droplet <$> v .: "id"
                    <*> attrs
                    <*> v .: "status"
                    <*> do networks <- v .: "networks"
                           networks .: "v4"

data IF = IF
        {
             _address :: Text
        ,    _addressType :: IPAddressType
        } deriving (Generic,Show)

instance FromJSON IF where
    parseJSON = withObject "IF" $ \v -> 
        IF <$> v.: "ip_address"
           <*> v.: "type"

address :: Lens' IF Text
address f s = _address s & f <&> \a -> s { _address = a }

addressType :: Lens' IF IPAddressType
--addressType f s = _addressType s & f <&> \a -> s { _addressType = a }
addressType = field' @"_addressType"

data IPAddressType = PublicIP
                   | OtherIP
                   deriving (Generic,Show)

_PublicIP :: Traversal' IPAddressType ()
_PublicIP = _Ctor' @"PublicIP"

instance FromJSON IPAddressType where
    parseJSON = withText "IPAddressType" $ \v -> 
        case v of
            "public" -> pure PublicIP
            _ -> pure OtherIP

data DropletStatus = New | Active | Off | Archive deriving (Show,Eq)

_New :: Traversal' DropletStatus ()
_New f = 
    \case New -> pure New <* f ()
          other -> pure other

_Active :: Traversal' DropletStatus ()
_Active f = 
    \case Active -> pure Active <* f ()
          other -> pure other

_Off :: Traversal' DropletStatus ()
_Off f = 
    \case Off -> pure Off <* f ()
          other -> pure other

_Archive :: Traversal' DropletStatus ()
_Archive f = 
    \case Archive -> pure Archive <* f ()
          other -> pure other

instance FromJSON DropletStatus where
    parseJSON = withText "Status" $ \v -> 
        case v of
            "new" -> pure New
            "active" -> pure Active
            "off" -> pure Off
            "archive" -> pure Archive
            _ -> empty

newtype WrappedDroplet = WrappedDroplet { getDroplet :: Droplet } deriving Show

instance FromJSON WrappedDroplet where
    parseJSON = withObject "WrappedDroplet" $ \v -> 
        WrappedDroplet <$> v .: "droplet"


newtype RegionSlug = RegionSlug { getRegionSlug :: Text } deriving (Show,Eq,Generic)

deriving newtype instance FromJSON RegionSlug 
deriving newtype instance ToJSON RegionSlug 

newtype Snapshots = Snapshots { getSnapshots :: [Snapshot] }

instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \v -> 
        Snapshots <$> v .: "snapshots"

type SnapshotId = Text

data Snapshot = Snapshot
              {
                _snapshotId :: Text
              , _snapshotName :: Text
              , _snapshotRegionSlugs :: [RegionSlug]
              } deriving Show

snapshotId :: Lens' Snapshot Text
snapshotId f s = _snapshotId s & f <&> \a -> s { _snapshotId = a }

snapshotName :: Lens' Snapshot Text
snapshotName f s = _snapshotName s & f <&> \a -> s { _snapshotName = a }

snapshotRegionSlugs :: Lens' Snapshot [RegionSlug]
snapshotRegionSlugs f s = _snapshotRegionSlugs s & f <&> \a -> s { _snapshotRegionSlugs = a }

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \v -> 
        Snapshot <$> v .: "id"
                 <*> v .: "name"
                 <*> v .: "regions"

data ActionStatus = ActionInProgress
                  | ActionCompleted
                  | ActionErrored
                  deriving Show
                   
instance FromJSON ActionStatus where
    parseJSON = withText "ActionStatus" $ \t -> 
        case t of
            "in-progress" -> pure ActionInProgress
            "completed" -> pure ActionCompleted
            "errored" -> pure ActionErrored
            _ -> empty

_ActionInProgress :: Traversal' ActionStatus ()
_ActionInProgress f = 
    \case ActionInProgress -> pure ActionInProgress <* f ()
          other -> pure other

_ActionCompleted :: Traversal' ActionStatus ()
_ActionCompleted f = 
    \case ActionCompleted -> pure ActionCompleted <* f ()
          other -> pure other

_ActionErrored :: Traversal' ActionStatus ()
_ActionErrored f = 
    \case ActionErrored -> pure ActionErrored <* f ()
          other -> pure other

data ActionType = RebootAction
                | PowerOffAction
                | ShutdownAction
                | SnapshotAction
                deriving Show

instance FromJSON ActionType where
    parseJSON = withText "ActionType" $ \t -> 
        case t of
            "shutdown" -> pure Thrifty.DO.ShutdownAction
            "reboot" -> pure RebootAction
            "power-off" -> pure PowerOffAction
            "snapshot" -> pure SnapshotAction
            _ -> empty

type ActionId = Integer

data Action = Action
            {
                _actionId :: ActionId
            ,   _actionStatus :: ActionStatus 
            ,   _actionType :: ActionType
            ,   _actionStartedAt :: Text
            ,   _actionCompletedAt :: Maybe Text
            ,   _actionRegionSlug :: Maybe Text
            ,   _actionResourceId :: Integer
            ,   _actionResourceType :: Text
            } deriving Show

actionId :: Lens' Action ActionId
actionId f s = _actionId s & f <&> \a -> s { _actionId = a }

actionStatus :: Lens' Action ActionStatus
actionStatus f s = _actionStatus s & f <&> \a -> s { _actionStatus = a }

actionType :: Lens' Action ActionType
actionType f s = _actionType s & f <&> \a -> s { _actionType = a }

actionRegionSlug :: Lens' Action (Maybe Text)
actionRegionSlug f s = _actionRegionSlug s & f <&> \a -> s { _actionRegionSlug = a }

instance FromJSON Action where
    parseJSON = withObject "Action" $ \v -> 
        Action <$> v .: "id"
               <*> v .: "status"
               <*> v .: "type"
               <*> v .: "started_at"
               <*> v .:? "isComplete_at"
               <*> v .:? "region_slug"
               <*> v .: "resource_id"
               <*> v .: "resource_type"

newtype WrappedAction = WrappedAction { getAction :: Action } deriving Show

instance FromJSON WrappedAction where
    parseJSON = withObject "WrappedAction" $ \v -> 
        WrappedAction <$> v .: "action"

droplets :: MonadIO m => Token -> m [Droplet]
droplets token = getDroplets <$> liftIO (doGET' "/v2/droplets" token)

shutdownDroplet :: Token -> DropletId -> IO Action
shutdownDroplet token dropletId0 =
    do WrappedAction a <- doPOST' 
                          (fromString ("/v2/droplets/"++ show dropletId0 ++"/actions"))
                          []
                          (object ["type" .= String "shutdown"])
                          token
       log ("Initiated shutdown action: " ++ show a)
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionCompleted)
                (action token (view actionId a))

type SnapshotName = Text 

createSnapshot :: Token -> SnapshotName -> DropletId -> IO Action
createSnapshot token name dropletId0 = 
    do WrappedAction a <- doPOST' 
                          (fromString ("/v2/droplets/"++ show dropletId0 ++"/actions"))
                          []
                          (object ["type" .= String "snapshot","name" .= name])
                          token
       log ("Initiated snapshot action: " ++ show a)
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionCompleted)
                (action token (view actionId a))

-- "No response body will be sent back, but the response code will indicate
-- success. Specifically, the response code will be a 204, which means that the
-- action was successful with no returned body data."
--
-- https://developers.digitalocean.com/documentation/v2/#delete-a-droplet
deleteDroplet :: Token -> DropletId -> IO ()
deleteDroplet token dropletId0 = 
    do doDELETE_' (fromString ("/v2/droplets/" ++show dropletId0)) token
       return ()

-- "A status of 204 will be given. This indicates that the request was processed successfully, but that no response body is needed."
deleteSnapshot :: Token -> SnapshotId -> IO ()
deleteSnapshot token snapshotId0 = 
    do doDELETE_' (fromString ("/v2/snapshots/" ++Data.Text.unpack snapshotId0)) token
       return ()

data PersistentAttributes = PersistentAttributes
                    {
                         _name :: Text
                    ,    _regionSlug :: RegionSlug
                    ,    _sizeSlug :: Text      
                    } deriving (Generic,Eq,Show,ToRecord,FromRecord)

persistenAttributesAliases :: Aliases _
persistenAttributesAliases =
     alias @"_name"       "name"
   . alias @"_regionSlug" "region_slug"
   . alias @"_sizeSlug"   "size_slug"
   $ unit

-- | Used only in Config object.
instance FromJSON PersistentAttributes where
    parseJSON = nominalRecordFromJSON persistenAttributesAliases

-- | Used only in Config object.
instance ToJSON PersistentAttributes where
    toJSON = nominalRecordToJSON persistenAttributesAliases

name :: Lens' PersistentAttributes Text
name f s = _name s & f <&> \a -> s { _name = a }

regionSlug :: Lens' PersistentAttributes RegionSlug
regionSlug f s = _regionSlug s & f <&> \a -> s { _regionSlug = a }

sizeSlug :: Lens' PersistentAttributes Text
sizeSlug f s = _sizeSlug s & f <&> \a -> s { _sizeSlug = a }

type ImageId = Integer

createDroplet :: Token -> PersistentAttributes -> ImageId -> IO Droplet
createDroplet token (PersistentAttributes {_name,_regionSlug,_sizeSlug}) imageId = 
    do WrappedDroplet d <- doPOST' 
                           ("/v2/droplets/") 
                           []
                           (object 
                             [
                                "name" .= _name,
                                "region" .= getRegionSlug _regionSlug,
                                "size" .= _sizeSlug,
                                "image" .= imageId
                             ])
                           token
       log ("Initiated droplet creation: " ++ show d)
       complete (dropletStatus._Void.united)
                (dropletStatus._Active)
                (droplet token (view dropletId d))


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

action :: Token -> ActionId -> IO Action
action token actionId0 = 
    do WrappedAction a <- doGET' (fromString ("/v2/actions/" ++ show actionId0)) token
       return a

droplet :: MonadIO m => Token -> DropletId -> m Droplet
droplet token dropletId0 = 
    do WrappedDroplet d <- liftIO (doGET' (fromString ("/v2/droplets/" ++ show dropletId0)) token)
       return d

snapshots :: MonadIO m => Token -> m [Snapshot]
snapshots token = getSnapshots <$> liftIO (doGET' "/v2/snapshots/?resource_type=droplet" token)

dropletMatches :: PersistentAttributes -> Droplet -> Bool 
dropletMatches attrs d =
    attrs == view dropletAttrs d   

snapshotMatches :: SnapshotName -> RegionSlug -> Snapshot -> Bool
snapshotMatches snapshotName0 regionSlug0 s =
       snapshotName0 == view snapshotName s
    && any (== regionSlug0) (view snapshotRegionSlugs s) 

baseURL :: AbsoluteURL
baseURL = fromString "https://api.digitalocean.com"

doGET' :: FromJSON a => RelativeURL -> Token -> IO a 
doGET' = doGET . extendAbsoluteURL baseURL

doPOST' :: (ToJSON body, FromJSON result) => RelativeURL -> [(Text,[Text])] -> body -> Token -> IO result
doPOST' = doPOST . extendAbsoluteURL baseURL 

doDELETE_' :: RelativeURL -> Token -> IO ()
doDELETE_' = doDELETE_ . extendAbsoluteURL baseURL
