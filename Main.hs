module Main where

import Thrifty.Main
import Thrifty
import Thrifty.DO

plugins :: [(String,ProviderPlugin)]
plugins = [("do", ProviderPlugin "DIGITALOCEAN_ACCESS_TOKEN" (SomeProvider . makeDO))]

main :: IO ()
main = defaultMain plugins
