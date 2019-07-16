module Main where

import Thrifty.Main
import Thrifty
import Thrifty.DO

plugins :: [(String,IO SomeProvider)]
plugins = [("do", tokenFromEnvironment "DIGITALOCEAN_ACCESS_TOKEN" (SomeProvider . makeDO))]

main :: IO ()
main = defaultMain plugins
