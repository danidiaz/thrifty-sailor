{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-#  OPTIONS_GHC -Wno-partial-type-signatures #-}
module Thrifty.Main (defaultMain,tokenFromEnvironment) where

import           Prelude hiding (log)

import           System.Directory
import           System.FilePath
import           System.Environment
import           Data.Aeson
import           Control.Monad.Except
import qualified Data.Aeson.Encode.Pretty
import           Options.Applicative
import           Control.Lens 
import qualified Options.Applicative as O
import qualified Data.ByteString.Lazy.Char8
import           Data.Text (Text)            
import qualified Data.Text             
import           GHC.Generics 
import           Data.Map(Map)
import qualified Data.Map
import qualified Data.ByteString.Lazy
import           System.IO (stdout)

import           Thrifty.Prelude
import           Thrifty.JSON
import           Thrifty
import           Thrifty.DO 
                         (
                             droplets
                         ,   snapshots
                         ,   moveUp
                         ,   moveDown
                         ,   NameRegionSize(..)
                         ,   RegionSlug(..)
                         ,   makeDO
                         ,   DOServer(..)
                         )

tokenFromEnvironment :: String -> (String -> SomeProvider) -> IO SomeProvider
tokenFromEnvironment variableName makeProvider =
  do token <- getEnv variableName
     return (makeProvider token)

xdgConfPath :: IO FilePath
xdgConfPath = do
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    log ("Looking for configuration file " ++ file ++ ".")
    return file

data Command = 
      Example 
    | Candidates
    | Status 
    | Up 
    | Down 
    deriving (Eq,Show)

data NameDesc = NameDesc { optionName :: String, optionDesc :: String }

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = 
            O.subparser 
          . mconcat
          $ [ 
              subcommand (pure Candidates)  
                         (NameDesc "candidates" 
                                   "Show candidates for snapshotification"),
              subcommand (pure Status)  
                         (NameDesc "status" 
                                   "Show current status of the target server"),
              subcommand (pure Up)
                         (NameDesc "up" 
                                   "Restores server from snapshot, deletes snapshot"),
              subcommand (pure Down)
                         (NameDesc "down" 
                                   "Shuts down server, makes snapshot, destroys server") ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p nd = 
        O.command (optionName nd) (infoHelpDesc p (optionDesc nd))

defaultMain :: [(String,IO SomeProvider)] -> IO ()
defaultMain (Data.Map.fromList -> plugins) = do
    command <- O.execParser parserInfo
    case command of
        Candidates ->
            do let Just makeProvider = Data.Map.lookup "do" plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                       do cs <- candidates 
                          Data.ByteString.Lazy.hPut stdout (encode cs)
        Status -> 
            do let Just makeProvider = Data.Map.lookup "do" plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup "do" confs
                            Just v = Data.Map.lookup "foo" selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        state <- serverState conf
                        print ("server is " ++ case state of
                            ServerIsDown _   -> "down"
                            ServerIsUp _ -> "up")
        Up ->
            do let Just makeProvider = Data.Map.lookup "do" plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup "do" confs
                            Just v = Data.Map.lookup "foo" selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        ServerIsDown action <- serverState conf
                        action
        Down ->
            do let Just makeProvider = Data.Map.lookup "do" plugins
               provider <- makeProvider
               case provider of
                   SomeProvider (Provider { candidates, serverState }) -> 
                     do confs <- load
                        let Just selectedProviderConfs = Data.Map.lookup "do" confs
                            Just v = Data.Map.lookup "foo" selectedProviderConfs
                            Data.Aeson.Success conf = fromJSON v
                        ServerIsUp action <- serverState conf
                        action
  where
    load :: IO (Map String (Map String Data.Aeson.Value))
    load = xdgConfPath >>= Data.Aeson.eitherDecodeFileStrict >>= liftError userError 

