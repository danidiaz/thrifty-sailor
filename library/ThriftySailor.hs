{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module ThriftySailor (
        Token
    ,   droplets

    ,   Droplet
    ,   dropletId
    ,   dropletName
    ,   regionSlug
    ,   dropletStatus

    ,   DropletStatus(..)
    ,   _New
    ,   _Active
    ,   _Off
    ,   _Archive 

    ,   snapshots

    ,   Snapshot
    ,   snapshotId
    ,   snapshotName
    ,   snapshotRegionSlugs

    ,   shutdown
    ,   snapshot
    ,   deleteDroplet
    ,   DropletCreation(..)
    ,   createDroplet
    ,   deleteSnapshot
    ) where

import           Data.Foldable
import           Data.Traversable
import           Data.Aeson
import           Data.Monoid
import           Network.Wreq
import           Control.Applicative
import           Control.Monad
import           Control.Lens hiding ((.=))
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text)            
import qualified Data.Text
import           Control.Exception
import           ThriftySailor.Delays
import           ThriftySailor.Prelude

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html
-- | https://developers.digitalocean.com/documentation/v2/
-- | https://developers.digitalocean.com/documentation/v2/#list-all-droplets

type Token = String

type RelUrl = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

newtype Droplets = Droplets { getDroplets :: [Droplet] } deriving Show

instance FromJSON Droplets where
    parseJSON = withObject "Droplets" $ \v -> 
        Droplets <$> v .: "droplets"

type DropletId = Integer

data Droplet = Droplet 
             {
                _dropletId :: DropletId
             ,  _dropletName :: Text
             ,  _regionSlug :: Text
             ,  _sizeSlug :: Text
             ,  _status :: DropletStatus
             } deriving Show

dropletId :: Lens' Droplet Integer
dropletId f s = _dropletId s & f <&> \a -> s { _dropletId = a }

dropletName :: Lens' Droplet Text
dropletName f s = _dropletName s & f <&> \a -> s { _dropletName = a }

regionSlug :: Lens' Droplet Text
regionSlug f s = _regionSlug s & f <&> \a -> s { _regionSlug = a }

dropletStatus :: Lens' Droplet DropletStatus
dropletStatus f s = _status s & f <&> \a -> s { _status = a }

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" $ \v -> 
        Droplet <$> v .: "id"
                <*> v .: "name"
                <*> do region <- v .: "region"
                       region .: "slug"
                <*> v .: "size_slug"
                <*> v .: "status"

data DropletStatus = New | Active | Off | Archive deriving (Show,Eq)

_New :: Traversal' DropletStatus ()
_New f = \case New -> pure New <* f ()
               other -> pure other

_Active :: Traversal' DropletStatus ()
_Active f = \case Active -> pure Active <* f ()
                  other -> pure other

_Off :: Traversal' DropletStatus ()
_Off f = \case Off -> pure Off <* f ()
               other -> pure other

_Archive :: Traversal' DropletStatus ()
_Archive f = \case Archive -> pure Archive <* f ()
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

newtype Snapshots = Snapshots { getSnapshots :: [Snapshot] }

instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \v -> 
        Snapshots <$> v .: "snapshots"

type SnapshotId = Text

data Snapshot = Snapshot
              {
                _snapshotId :: Text
              , _snapshotName :: Text
              , _snapshotRegionSlugs :: [Text]
              } deriving Show

snapshotId :: Lens' Snapshot Text
snapshotId f s = _snapshotId s & f <&> \a -> s { _snapshotId = a }

snapshotName :: Lens' Snapshot Text
snapshotName f s = _snapshotName s & f <&> \a -> s { _snapshotName = a }

snapshotRegionSlugs :: Lens' Snapshot [Text]
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
_ActionInProgress f = \case ActionInProgress -> pure ActionInProgress <* f ()
                            other -> pure other

_ActionCompleted :: Traversal' ActionStatus ()
_ActionCompleted f = \case ActionCompleted -> pure ActionCompleted <* f ()
                           other -> pure other

_ActionErrored :: Traversal' ActionStatus ()
_ActionErrored f = \case ActionErrored -> pure ActionErrored <* f ()
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

shutdown :: Token -> DropletId -> IO Action
shutdown token dropletId' =
    do WrappedAction a <- doPOST ("/v2/droplets/"++ show dropletId' ++"/actions")
                                 [("type",["shutdown"])]
                                 token
       putStrLn $ "Initiated shutdown action: " ++ show a
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionInProgress)
                (action token (view actionId a))

type SnapshotName = Text 

snapshot :: Token -> DropletId -> SnapshotName -> IO Action
snapshot token dropletId0 name = 
    do WrappedAction a <- doPOST ("/v2/droplets/"++ show dropletId0 ++"/actions")
                                 [("type",["snapshot"]),("name",[name])]
                                 token
       putStrLn $ "Initiated snapshot action: " ++ show a
       complete (actionStatus._ActionErrored)
                (actionStatus._ActionInProgress)
                (action token (view actionId a))

deleteDroplet :: Token -> DropletId -> IO ()
deleteDroplet token dropletId0 = 
    do doDELETE ("/v2/droplets/" ++show dropletId0) token
       return ()

deleteSnapshot :: Token -> SnapshotId -> IO ()
deleteSnapshot token snapshotId0 = 
    do doDELETE ("/v2/snapshots/" ++Data.Text.unpack snapshotId0) token
       return ()

data DropletCreation = DropletCreation
                   {
                        _dropletCreationName :: Text
                   ,    _dropletCreationRegionSlug :: Text
                   ,    _dropletCreationSizeSlug :: Text      
                   ,    _dropletCreationSnapshotId :: Integer -- public images currently not supported
                   } deriving Show

createDroplet :: Token -> DropletCreation -> IO Droplet
createDroplet token dc = 
    do WrappedDroplet d <- doPOST ("/v2/droplets/") 
                                  [("name",[_dropletCreationName dc])
                                  ,("region",[_dropletCreationRegionSlug dc])
                                  ,("size",[_dropletCreationSizeSlug dc])
                                  ,("image",[Data.Text.pack . show $ _dropletCreationSnapshotId dc])
                                  ]
                                  token
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
                     putStrLn $ "Checked again, and the result was: " ++ show a
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

authorized :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authorized token = set auth (Just (oauth2Bearer (Char8.pack token)))

doGET :: FromJSON a => RelUrl -> Token -> IO a 
doGET relUrl token =  
   do r <- getWith (authorized token defaults) (baseUrl ++ relUrl)
      view responseBody <$> asJSON r

doPOST :: FromJSON a => RelUrl -> [(Text,[Text])] -> Token -> IO a 
doPOST relUrl params token =  
   do let options = alaf Endo foldMap (\(k,v) -> set (param k) v) params
                  . authorized token 
                  $ defaults
      r <- postWith options (baseUrl ++ relUrl) (toJSON ())
      view responseBody <$> asJSON r

doDELETE :: RelUrl -> Token -> IO ()
doDELETE relUrl token = 
    do deleteWith (authorized token defaults) (baseUrl ++ relUrl)
       pure ()

