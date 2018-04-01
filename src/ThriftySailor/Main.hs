{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module ThriftySailor.Main where

import System.Directory
import System.FilePath
import System.Environment
import Control.Monad.Except 
import Data.Aeson
import Data.Function ((&))
import Data.Bifunctor
import Options.Applicative
import qualified Options.Applicative as O

data Config = Config { doTokenEnvVar :: String } deriving (Eq,Show)

sample :: Config
sample = Config "DIGITAL_OCEAN_TOKEN"

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config
         <$> v .: "DIGITAL_OCEAN_TOKEN_ENV_VARIABLE"

data Command = Init | Ask | Up | Down deriving (Eq,Show)

data NameDesc = NameDesc { name :: String, desc :: String }

parserInfo :: O.ParserInfo Command
parserInfo = 
    let parser = 
            O.subparser . mconcat $
            [ subcommand (pure Init) $ NameDesc "init" "inits stuff"
            , subcommand (pure Ask) $ NameDesc "ask" "asks stuff"
            , subcommand (pure Up) $ NameDesc "up" "ups stuff"
            , subcommand (pure Down) $ NameDesc "down" "downs stuff"
            ]
     in infoHelpDesc parser "Main options."
  where
    infoHelpDesc :: O.Parser a -> String -> O.ParserInfo a
    infoHelpDesc p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

    subcommand :: O.Parser a -> NameDesc -> Mod CommandFields a
    subcommand p (NameDesc {name,desc}) = O.command name (infoHelpDesc p desc)

data Msgs = Msgs 
    {
        lookingForConfFile :: FilePath -> String,
        tokenNotFound :: String -> String
    }

msgs :: Msgs
msgs = 
    Msgs 
    (\file -> "Looking for configuration file " ++ file ++ ".")
    (\var -> "Token " ++ var ++ " not found in environment.") 

defaultMain :: IO ()
defaultMain = defaultMainWith msgs

defaultMainWith :: Msgs -> IO ()
defaultMainWith msgs = do 
    command <- O.execParser parserInfo
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    putStrLn $ lookingForConfFile msgs file
    conf <- do e <- eitherDecodeFileStrict' file
               eitherError userError e
    print conf
    let Config {doTokenEnvVar} = conf
    doToken <- do m <- lookupEnv doTokenEnvVar 
                  maybeError (userError (tokenNotFound msgs doTokenEnvVar)) m
    print $ conf
    return ()

eitherError :: MonadError e' m => (e -> e') -> Either e r -> m r 
eitherError f = either throwError return . first f

maybeError :: MonadError e' m => e' -> Maybe r -> m r 
maybeError e' = maybe (throwError e') return
