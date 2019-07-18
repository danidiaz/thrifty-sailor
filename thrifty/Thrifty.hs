{-# LANGUAGE ExistentialQuantification #-}
module Thrifty (Provider(..),ServerState(..),SomeProvider(..),IPAddress(..)) where

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

