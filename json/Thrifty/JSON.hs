{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Thrifty.JSON (
        Aliases
    ,   alias
    ,   nominalRecordFromJSON
    ,   nominalRecordToJSON
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Proxy
import           Data.RBR
import           Data.SOP
import           Data.SOP.NP
import           Data.Functor.Compose
import           Data.String (fromString)
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits

type Aliases t = Record (K String) t

alias :: forall k v t. Insertable k v t => String -> Aliases t -> Aliases (Insert k v t)
alias = insert @k @v . K

nominalRecordFromJSON 
    :: forall r c flat name m package stuff
                      . (NamedDataType r,
                         FromRecord r, 
                         RecordCode r ~ c, 
                         Productlike '[] c flat, 
                         All FromJSON flat) 
    => Aliases c
    -> Data.Aeson.Value 
    -> Data.Aeson.Types.Parser r
nominalRecordFromJSON aliases = 
    let giveFieldName (K alias) (Compose f) = Compose (\o -> Data.Aeson.Types.explicitParseField f o (fromString alias))
        parsers = cpure_NP (Proxy @FromJSON) (Compose parseJSON)
        Compose parser = fromNP <$> sequence_NP (liftA2_NP giveFieldName (toNP aliases) parsers)
     in withObject (getRecordName (Proxy @r)) $ \o -> fromRecord <$> parser o

nominalRecordToJSON 
    :: forall r c flat. (ToRecord r, 
                         RecordCode r ~ c, 
                         Productlike '[] c flat, 
                         All ToJSON flat) 
    => Aliases c
    -> r
    -> Data.Aeson.Value
nominalRecordToJSON aliases r = 
    let giveFieldName (K alias) (I fieldValue) = K (fromString alias .= fieldValue)
        pairs = 
            hcliftA2 (Proxy @ToJSON) giveFieldName (toNP aliases) (toNP (toRecord r))
     in object (hcollapse pairs)

class NamedDataType r where
    getRecordName :: Proxy r -> String 

instance forall r name m package stuff. (Generic r, Rep r ~ D1 ('MetaData name m package 'False) stuff, KnownSymbol name) => NamedDataType r where
    getRecordName _ = symbolVal (Proxy @name) 

