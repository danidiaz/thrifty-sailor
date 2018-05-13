module ThriftySailor where

import           Data.Aeson
import           Network.Wreq
import qualified Data.ByteString.Char8 as Char8
import           Control.Lens

-- | http://hackage.haskell.org/package/req-1.0.0/docs/Network-HTTP-Req.html
-- | https://developers.digitalocean.com/documentation/v2/
-- | https://developers.digitalocean.com/documentation/v2/#list-all-droplets

type Token = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

listAllDroplets :: Token -> IO Value
listAllDroplets token =
    let opts = set auth (Just (oauth2Bearer (Char8.pack token))) defaults
        relpath = "/v2/droplets"
     in do r <- getWith opts (baseUrl ++ relpath)
           r' <- asValue r
           return $ view responseBody r'


