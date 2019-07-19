{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Thrifty (Provider(..),ServerState(..),SomeProvider(..),IPAddress(..),withSomeProvider) where

import Data.Aeson
import Data.Text
import Data.List.NonEmpty

data Provider server = Provider {
        candidates :: IO [server],
        serverState :: server -> IO (ServerState IO)
    }

data ServerState m = 
      ServerIsDown (m (NonEmpty IPAddress))
    | ServerIsUp (m ())

newtype IPAddress = IPAddress { getIPAddress :: Text } deriving (Show,Eq)

data SomeProvider = 
    forall server. (FromJSON server, ToJSON server) => SomeProvider (Provider server)

withSomeProvider :: forall r . SomeProvider -> (forall server. (FromJSON server, ToJSON server) => Provider server -> r) -> r
withSomeProvider (SomeProvider provider) callback = callback provider
    
