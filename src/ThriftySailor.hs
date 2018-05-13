{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ThriftySailor (
        Token
    ,   Droplet(..)
    ,   DropletStatus(..)
    ,   droplets
    ,   Snapshot(..)
    ,   snapshots
    ) where

import           Data.Foldable
import           Data.Traversable
import           Data.Aeson
import           Network.Wreq
import           Control.Applicative
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
                id :: Integer
             ,  name :: Text
             ,  regionSlug :: Text
             ,  status :: DropletStatus
             } deriving Show

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" $ \v -> 
        Droplet <$> v .: "id"
                <*> v .: "name"
                <*> do region <- v .: "region"
                       region .: "slug"
                <*> v .: "status"

data DropletStatus = New | Active | Off | Archive deriving (Show,Eq)

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
                id :: Text
              , name :: Text
              , regionSlugs :: [Text]
              } deriving Show

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


