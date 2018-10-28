module Thrifty.Network (
        Token
    ,   doGET
    ,   doPOST
    ,   doDELETE
    ) where

import           Data.Text(Text)
import qualified Data.ByteString.Char8 as Char8
import           Data.Aeson
import           Data.Monoid
import           Control.Lens
import           Network.Wreq

type Token = String

type RelUrl = String

baseUrl :: String
baseUrl = "https://api.digitalocean.com"

authorized :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authorized token = set auth (Just (oauth2Bearer (Char8.pack token)))

doGET :: FromJSON a => RelUrl -> Token -> IO a 
doGET relUrl token =  
   do r <- getWith (authorized token defaults) (baseUrl ++ relUrl)
      view responseBody <$> asJSON r

doPOST :: FromJSON a => RelUrl -> [(Text,[Text])] -> Token -> IO a 
doPOST relUrl params token =  
   do let options = alaf Endo foldMap (\(k,v) -> set (param k) v) params
                  . authorized token 
                  $ defaults
      r <- postWith options (baseUrl ++ relUrl) (toJSON ())
      view responseBody <$> asJSON r

doDELETE :: RelUrl -> Token -> IO ()
doDELETE relUrl token = 
    do deleteWith (authorized token defaults) (baseUrl ++ relUrl)
       pure ()

