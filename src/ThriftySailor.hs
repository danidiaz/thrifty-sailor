module ThriftySailor where

import Data.Aeson
import Network.HTTP.Req

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html

type Token = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

listAllDroplets :: Token -> IO Value
listAllDroplets = undefined


