{-# LANGUAGE NamedFieldPuns #-}
module ThriftySailor.Main where

import System.Directory
import Options.Applicative
import qualified Options.Applicative as O

data Command = Init | Ask | Up | Down deriving (Eq,Ord,Show)

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = O.subparser . mconcat $
            [ subcommand (pure Init) $ NameDesc "init" "inits stuff"
            , subcommand (pure Ask) $ NameDesc "ask" "asks stuff"
            , subcommand (pure Up) $ NameDesc "up" "ups stuff"
            , subcommand (pure Down) $ NameDesc "down" "downs stuff"
            ]
     in infoHelpDesc parser "Main options."

data NameDesc = NameDesc { name :: String, desc :: String }

infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
subcommand p (NameDesc {name,desc}) = O.command name (infoHelpDesc p desc)

defaultMain :: IO ()
defaultMain = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    print xdg
    command <- O.execParser parserInfo
    print $ command
    return ()


