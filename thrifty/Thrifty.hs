{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Thrifty (Provider(..),ServerState(..),SomeProvider(..),IPAddress(..),withSomeProvider,StartupAction(..),ShutdownAction(..)) where

import Data.Aeson
import Data.Text
import Data.List.NonEmpty

data Provider server = Provider {
        candidates :: IO [server],
        serverState :: server -> IO ServerState
    }

data ServerState = 
      ServerIsDown (StartupAction (NonEmpty IPAddress))
    | ServerIsUp ShutdownAction

newtype StartupAction a = StartupAction { startupServer :: IO a }

newtype ShutdownAction = ShutdownAction { shutdownServer :: IO () }

newtype IPAddress = IPAddress { getIPAddress :: Text } deriving (Show,Eq)

data SomeProvider = 
    forall server. (FromJSON server, ToJSON server) => SomeProvider (Provider server)

withSomeProvider :: forall r . SomeProvider -> (forall server. (FromJSON server, ToJSON server) => Provider server -> r) -> r
withSomeProvider (SomeProvider provider) callback = callback provider
    
