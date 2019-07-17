{-# LANGUAGE OverloadedStrings #-}
module Main where

import Thrifty.Main (defaultMain,ProviderName(..),tokenFromEnvironment)
import Thrifty (SomeProvider(..))
import Thrifty.DO (makeDO)

plugins :: [(ProviderName, IO SomeProvider)]
plugins = [(ProviderName "do", tokenFromEnvironment "DIGITALOCEAN_ACCESS_TOKEN" (SomeProvider . makeDO))]

main :: IO ()
main = defaultMain plugins
