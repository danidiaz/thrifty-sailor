{-# LANGUAGE OverloadedStrings #-}
module Thrifty.Network (
        Token
    ,   doGET
    ,   doPOST
    ,   doDELETE_
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

doPOST :: (ToJSON body, FromJSON result) => AbsoluteURL -> [(Text,[Text])] -> body -> Token -> IO result
doPOST url params body token =  
   do let options = alaf Endo foldMap (\(k,v) -> set (param k) v) params
                  . authorized token 
                  $ defaults
      r <- postWith options url (toJSON body)
      view responseBody <$> asJSON r

doDELETE_ :: AbsoluteURL -> Token -> IO ()
doDELETE_ url token = 
    do deleteWith (authorized token defaults) url
       pure ()

-- Delete returning a JSON body
doDELETE :: (FromJSON result) => AbsoluteURL -> Token -> IO result
doDELETE url token = 
    do let options = set (header "Accept") ["application/json"] 
                   . authorized token
                   $ defaults
       r <- deleteWith options url
       view responseBody <$> asJSON r

authorized :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authorized token = set auth (Just (oauth2Bearer (Char8.pack token)))

