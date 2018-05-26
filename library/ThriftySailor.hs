{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

    ,   dropletsWithName
    ,   dropletsWithRegion
    ) where

import           Data.Foldable
import           Data.Traversable
import           Data.Aeson
import           Network.Wreq
import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text)            
import qualified Data.Text             as Text
import           Control.Lens

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

data Droplet = Droplet 
             {
                _dropletId :: Integer
             ,  _dropletName :: Text
             ,  _regionSlug :: Text
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

newtype Snapshots = Snapshots { getSnapshots :: [Snapshot] }

instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \v -> 
        Snapshots <$> v .: "snapshots"

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

droplets :: Token -> IO [Droplet]
droplets token = getDroplets <$> doGET "/v2/droplets" token

snapshots :: Token -> IO [Snapshot]
snapshots token = getSnapshots <$> doGET "/v2/snapshots/?resource_type=droplet" token

authorized :: String -> Network.Wreq.Options
authorized token = set auth (Just (oauth2Bearer (Char8.pack token))) defaults

doGET :: FromJSON a => RelUrl -> Token -> IO a 
doGET relUrl token =  
   do r <- getWith (authorized token) (baseUrl ++ relUrl)
      view responseBody <$> asJSON r

dropletsWithName :: Text -> [Droplet] -> [Droplet]
dropletsWithName name = toListOf (folded.filtered (has (dropletName.only name))) 

dropletsWithRegion :: Text -> [Droplet] -> [Droplet]
dropletsWithRegion region = toListOf (folded.filtered (has (regionSlug.only region))) 