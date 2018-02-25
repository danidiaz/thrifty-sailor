module ThriftySailor.Main where

import System.Directory

defaultMain :: IO ()
defaultMain = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    print xdg
    return ()

