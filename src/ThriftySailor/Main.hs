module ThriftySailor.Main where

import System.Directory
import Options.Applicative

data Options = Init | Up | Down deriving (Eq,Ord,Show)

defaultMain :: IO ()
defaultMain = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    print xdg
    return ()


