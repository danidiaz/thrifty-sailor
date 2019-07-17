module Thrifty.Network (
        Token
    ,   doGET
    ,   doPOST
    ,   doDELETE
    ,   extendAbsoluteURL
    ,   AbsoluteURL
    ,   RelativeURL
    ) where

import           Data.Text(Text)
import qualified Data.ByteString.Char8 as Char8
import           Data.Aeson
import           Data.Monoid
import           Control.Lens
import           Network.Wreq

type Token = String

type RelativeURL = String

type AbsoluteURL = String

extendAbsoluteURL :: AbsoluteURL -> RelativeURL -> AbsoluteURL
extendAbsoluteURL = (++)

doGET :: FromJSON a => AbsoluteURL -> Token -> IO a 
doGET url token =  
   do r <- getWith (authorized token defaults) url
      view responseBody <$> asJSON r

doPOST :: FromJSON a => AbsoluteURL -> [(Text,[Text])] -> Token -> IO a 
doPOST url params token =  
   do let options = alaf Endo foldMap (\(k,v) -> set (param k) v) params
                  . authorized token 
                  $ defaults
      r <- postWith options url (toJSON ())
      view responseBody <$> asJSON r

doDELETE :: AbsoluteURL -> Token -> IO ()
doDELETE url token = 
    do deleteWith (authorized token defaults) url
       pure ()

authorized :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authorized token = set auth (Just (oauth2Bearer (Char8.pack token)))

