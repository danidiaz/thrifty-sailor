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

module Thrifty.JSON (
        Aliases
    ,   alias
    ,   recordFromJSON
    ,   recordToJSON
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Proxy
import           Data.RBR
import           Data.SOP
import           Data.SOP.NP
import           Data.Functor.Compose
import           Data.String (fromString)

type Aliases t = Record (K String) t

alias :: forall k v t. Insertable k v t => String -> Aliases t -> Aliases (Insert k v t)
alias = insert @k @v . K

recordFromJSON 
    :: forall r c flat. (FromRecord r, 
                         RecordCode r ~ c, 
                         Productlike '[] c flat, 
                         All FromJSON flat) 
    => Record (K String) c
    -> Data.Aeson.Value 
    -> Data.Aeson.Types.Parser r
recordFromJSON aliases = 
    let mapKSS (K name) (Compose pf) = Compose (\o -> Data.Aeson.Types.explicitParseField pf o (fromString name))
        pr = cpure_NP (Proxy @FromJSON) (Compose parseJSON)
        Compose parser = fromNP <$> sequence_NP (liftA2_NP mapKSS (toNP @c aliases) pr)
     in withObject "obj" $ \o -> fromRecord <$> parser o

recordToJSON 
    :: forall r c flat. (ToRecord r, 
                         RecordCode r ~ c, 
                         Productlike '[] c flat, 
                         All ToJSON flat) 
    => Record (K String) c
    -> r
    -> Data.Aeson.Value
recordToJSON aliases r = 
    let pairs = 
            hcliftA2 (Proxy @ToJSON) (\(K a) (I x) -> K ((fromString a) .= x)) (toNP aliases) (toNP (toRecord r))
     in object (hcollapse pairs)

