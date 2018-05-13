{-# LANGUAGE OverloadedStrings #-}
module ThriftySailor where

import           Data.Aeson
import           Network.Wreq
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text)            
import qualified Data.Text             as Text
import           Control.Lens

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html
-- | https://developers.digitalocean.com/documentation/v2/
-- | https://developers.digitalocean.com/documentation/v2/#list-all-droplets

type Token = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"


newtype Droplets = Droplets { getDroplets :: [Droplet] }

instance FromJSON Droplets where
    parseJSON = withObject "Droplets" $ \v -> 
        Droplets <$> v .: "droplets"

data Droplet = Droplet 
             {
                id :: Integer
             ,  name :: Text
             ,  regionSlug :: Text
             ,  status :: Text
             } deriving Show

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" $ \v -> 
        Droplet <$> v .: "id"
                <*> v .: "name"
                <*> do region <- v .: "region"
                       region .: "slug"
                <*> v .: "status"

droplets :: Token -> IO [Droplet]
droplets token =
    let opts = set auth (Just (oauth2Bearer (Char8.pack token))) defaults
        relpath = "/v2/droplets"
     in do r <- getWith opts (baseUrl ++ relpath)
           getDroplets . view responseBody <$> asJSON r


