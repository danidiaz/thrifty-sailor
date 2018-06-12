{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ThriftySailor (
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

    ,   NameRegionSize(..)
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
    ) where

import           Prelude hiding (log)
import           Data.Foldable
import           Data.Traversable
import           Data.Aeson
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Lens hiding ((.=))
import           Data.Text (Text)            
import qualified Data.Text
import           Control.Exception
import qualified GHC.Generics as GHC
import           Generics.SOP

import           ThriftySailor.Prelude
import           ThriftySailor.Delays
import           ThriftySailor.JSON
import           ThriftySailor.Network (doGET,doPOST,doDELETE,Token)

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
             ,  _dropletAttrs :: NameRegionSize
             ,  _status :: DropletStatus
             ,  _networks :: [IF]
             } deriving Show

dropletId :: Lens' Droplet Integer
dropletId f s = _dropletId s & f <&> \a -> s { _dropletId = a }

dropletAttrs :: Lens' Droplet NameRegionSize
dropletAttrs f s = _dropletAttrs s & f <&> \a -> s { _dropletAttrs = a }

dropletStatus :: Lens' Droplet DropletStatus
dropletStatus f s = _status s & f <&> \a -> s { _status = a }

networks :: Lens' Droplet [IF]
networks f s = _networks s & f <&> \a -> s { _networks = a }

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" $ \v -> 
        let attrs = NameRegionSize <$> v .: "name"
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
        } deriving Show

instance FromJSON IF where
    parseJSON = withObject "IF" $ \v -> 
        IF <$> v.: "ip_address"
           <*> v.: "type"

address :: Lens' IF Text
address f s = _address s & f <&> \a -> s { _address = a }

addressType :: Lens' IF IPAddressType
addressType f s = _addressType s & f <&> \a -> s { _addressType = a }

data IPAddressType = PublicIP
                   | OtherIP
                   deriving Show

_PublicIP :: Traversal' IPAddressType ()
_PublicIP f = 
    \case PublicIP -> pure PublicIP <* f ()
          other -> pure other

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


newtype RegionSlug = RegionSlug { getRegionSlug :: Text } deriving (Show,Eq,GHC.Generic)

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
            "shutdown" -> pure ShutdownAction
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

droplets :: Token -> IO [Droplet]
droplets token = getDroplets <$> doGET "/v2/droplets" token

shutdownDroplet :: Token -> DropletId -> IO Action
shutdownDroplet token dropletId0 =
    do WrappedAction a <- doPOST ("/v2/droplets/"++ show dropletId0 ++"/actions")
                                 [("type",["shutdown"])]
                                 token
       log ("Initiated shutdown action: " ++ show a)
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionCompleted)
                (action token (view actionId a))

type SnapshotName = Text 

createSnapshot :: Token -> SnapshotName -> DropletId -> IO Action
createSnapshot token name dropletId0 = 
    do WrappedAction a <- doPOST ("/v2/droplets/"++ show dropletId0 ++"/actions")
                                 [("type",["snapshot"]),("name",[name])]
                                 token
       log ("Initiated snapshot action: " ++ show a)
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionCompleted)
                (action token (view actionId a))

deleteDroplet :: Token -> DropletId -> IO ()
deleteDroplet token dropletId0 = 
    do doDELETE ("/v2/droplets/" ++show dropletId0) token
       return ()

deleteSnapshot :: Token -> SnapshotId -> IO ()
deleteSnapshot token snapshotId0 = 
    do doDELETE ("/v2/snapshots/" ++Data.Text.unpack snapshotId0) token
       return ()

data NameRegionSize = NameRegionSize
                    {
                         _name :: Text
                    ,    _regionSlug :: RegionSlug
                    ,    _sizeSlug :: Text      
                    } deriving (GHC.Generic,Eq,Show)

instance Generic NameRegionSize
instance HasDatatypeInfo NameRegionSize

nameRegionSizeAliases :: AliasesFor (FieldNamesOf NameRegionSize)
nameRegionSizeAliases =
      alias @"_name"       "name"
   :* alias @"_regionSlug" "region_slug"
   :* alias @"_sizeSlug"   "size_slug"
   :* Nil

-- | Used only in Config object.
instance FromJSON NameRegionSize where
    parseJSON = recordFromJSON nameRegionSizeAliases

-- | Used only in Config object.
instance ToJSON NameRegionSize where
    toJSON = recordToJSON nameRegionSizeAliases

name :: Lens' NameRegionSize Text
name f s = _name s & f <&> \a -> s { _name = a }

regionSlug :: Lens' NameRegionSize RegionSlug
regionSlug f s = _regionSlug s & f <&> \a -> s { _regionSlug = a }

sizeSlug :: Lens' NameRegionSize Text
sizeSlug f s = _sizeSlug s & f <&> \a -> s { _sizeSlug = a }

type ImageId = Integer

createDroplet :: Token -> NameRegionSize -> ImageId -> IO Droplet
createDroplet token (NameRegionSize {_name,_regionSlug,_sizeSlug}) imageId = 
    do WrappedDroplet d <- doPOST ("/v2/droplets/") 
                                  [("name",[_name])
                                  ,("region",[getRegionSlug _regionSlug])
                                  ,("size",[_sizeSlug])
                                  ,("image",[Data.Text.pack . show $ imageId])
                                  ]
                                  token
       log ("Initiated droplet creation: " ++ show d)
       complete (dropletStatus._Void.united)
                (dropletStatus._Active)
                (droplet token (view dropletId d))

complete :: Show a 
         => (Fold a ()) -- ^ error check
         -> (Fold a ()) -- ^ completion check
         -> IO a 
         -> IO a
complete errCheck doneCheck action =
    do retries <- effects
                . giveUp (seconds 360)
                . retrying (waits (seconds 2) (factor 1.5) (seconds 15)) 
                $ do a <- action
                     log ("Checked again, and the result was: " ++ show a)
                     when (has errCheck a) 
                          (throwIO (userError ("Action error.")))
                     pure $ if has doneCheck a
                                then Right a
                                else Left ()
       eitherError (const (userError ("Timeout waiting."))) retries

action :: Token -> ActionId -> IO Action
action token actionId0 = 
    do WrappedAction a <- doGET ("/v2/actions/" ++ show actionId0) token
       return a

droplet :: Token -> DropletId -> IO Droplet
droplet token dropletId0 = 
    do WrappedDroplet d <- doGET ("/v2/droplets/" ++ show dropletId0) token
       return d

snapshots :: Token -> IO [Snapshot]
snapshots token = getSnapshots <$> doGET "/v2/snapshots/?resource_type=droplet" token


