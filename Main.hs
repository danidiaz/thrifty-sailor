{-# LANGUAGE OverloadedStrings #-}
module Main where

import Thrifty.Main (defaultMain,ProviderName(..),tokenFromEnvironment)
import Thrifty (SomeProvider(..))
import Thrifty.DO (makeDO)
import Thrifty.Hetzner (makeHetzner)

plugins :: [(ProviderName, IO SomeProvider)]
plugins = 
    [   (ProviderName "do", tokenFromEnvironment "DIGITALOCEAN_ACCESS_TOKEN" (SomeProvider . makeDO)),
        (ProviderName "hetzner", tokenFromEnvironment "HCLOUD_TOKEN" (SomeProvider . makeHetzner))
    ]

main :: IO ()
main = defaultMain plugins
