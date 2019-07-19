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
                _foo :: Text
            } deriving (Show,Generic,FromRecord,ToRecord)

hetznerServerAliases :: Aliases _
hetznerServerAliases =
     alias @"_foo" "foo"
   $ unit

instance FromJSON HetznerServer where
    parseJSON = nominalRecordFromJSON hetznerServerAliases

instance ToJSON HetznerServer where
    toJSON = recordToJSON hetznerServerAliases

makeHetzner :: Token -> Provider HetznerServer  
makeHetzner token = Provider undefined undefined

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
--             ,  _dropletAttrs :: NameRegionSize
             ,  _status :: DropletStatus
             ,  _networks :: [IF]
             } deriving (Generic,Show)

instance FromJSON Droplet where
    parseJSON = withObject "Droplet" \v -> 
      Droplet <$> v .: "id"
              <*> v .: "status"
              <*> do publicNetworks <- v .: "public_net"
                     publicIPv4Networks <- publicNetworks .: "ipv4"
                     ip <- publicIPv4Networks .: "ip"
                     return [IF ip]

dropletId :: Lens' Droplet Integer
dropletId f s = _dropletId s & f <&> \a -> s { _dropletId = a }

dropletStatus :: Lens' Droplet DropletStatus
dropletStatus = field' @"_status"

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

baseURL :: AbsoluteURL
baseURL = fromString "https://api.hetzner.cloud"

doGET' :: FromJSON a => RelativeURL -> Token -> IO a 
doGET' = doGET . extendAbsoluteURL baseURL

doPOST' :: (ToJSON body, FromJSON result) => RelativeURL -> [(Text,[Text])] -> body -> Token -> IO result
doPOST' = doPOST . extendAbsoluteURL baseURL 

doDELETE' :: RelativeURL -> Token -> IO ()
doDELETE' = doDELETE . extendAbsoluteURL baseURL

