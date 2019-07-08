{-# LANGUAGE ExistentialQuantification #-}
module Thrifty (Provider(..),ServerState(..),SomeProvider(..)) where

import Data.Aeson

data Provider server = Provider {
        listServers :: IO [server],
        serverState :: server -> ServerState IO
    }

data ServerState m = 
      Up (m ())
    | Down (m ())

data SomeProvider = 
    forall server. (FromJSON server, ToJSON server) => SomeProvider (Provider server)
