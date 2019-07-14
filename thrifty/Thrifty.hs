{-# LANGUAGE ExistentialQuantification #-}
module Thrifty (Provider(..),ServerState(..),SomeProvider(..)) where

import Data.Aeson

data Provider server = Provider {
        candidates :: IO [server],
        serverState :: server -> IO (ServerState IO)
    }

data ServerState m = 
      ServerIsDown (m ())
    | ServerIsUp (m ())

data SomeProvider = 
    forall server. (FromJSON server, ToJSON server) => SomeProvider (Provider server)
