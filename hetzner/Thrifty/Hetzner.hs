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

baseURL :: AbsoluteURL
baseURL = fromString "https://api.hetzner.cloud"

doGET' :: FromJSON a => RelativeURL -> Token -> IO a 
doGET' = doGET . extendAbsoluteURL baseURL

doPOST' :: (ToJSON body, FromJSON result) => RelativeURL -> [(Text,[Text])] -> body -> Token -> IO result
doPOST' = doPOST . extendAbsoluteURL baseURL 

doDELETE' :: RelativeURL -> Token -> IO ()
doDELETE' = doDELETE . extendAbsoluteURL baseURL

