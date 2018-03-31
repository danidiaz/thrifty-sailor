{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


defaultMain :: IO ()
defaultMain = do
    command <- O.execParser parserInfo
    xdg <- getXdgDirectory XdgConfig "thrifty-sailor" 
    let file = xdg </> "config.json" 
    putStrLn $ "Looking for configuration file " ++ file
    conf <- eitherDecodeFileStrict' file >>= eitherError userError
    print conf
    let Config {doTokenEnvVar} = conf
        tokenNotFoundMsg = "Token " ++ doTokenEnvVar ++ " not found in environment."
    doToken <- lookupEnv doTokenEnvVar >>= maybeError (userError tokenNotFoundMsg)
    print $ conf
    return ()


eitherError :: MonadError e' m => (e -> e') -> Either e r -> m r 
eitherError f = either throwError return . first f

maybeError :: MonadError e' m => e' -> Maybe r -> m r 
maybeError e' = maybe (throwError e') return
