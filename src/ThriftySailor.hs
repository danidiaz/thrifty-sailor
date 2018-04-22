module ThriftySailor where

import Data.Aeson
import Network.Wreq

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html
-- | https://developers.digitalocean.com/documentation/v2/
-- | https://developers.digitalocean.com/documentation/v2/#list-all-droplets

type Token = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

listAllDroplets :: Token -> IO Value
listAllDroplets = undefined


