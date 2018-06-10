{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module ThriftySailor.JSON (
        AliasesFor
    ,   FieldNamesOf
    ,   alias
    ,   recordFromJSON
    ,   recordToJSON
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)            
import qualified Data.Text as Text
import           GHC.TypeLits (Symbol)
import qualified GHC.Generics as GHC
import           Generics.SOP
import           Generics.SOP.NP
import qualified Generics.SOP.Type.Metadata as M

type AliasesFor ns = NP (K Text) ns

alias :: forall (k :: Symbol). Text -> K Text k
alias = K

recordFromJSON :: forall r xs ns.
                  (IsProductType r xs, 
                   HasDatatypeInfo r,
                   FieldNamesOf r ~ ns,
                   AllZip (LiftedCoercible (K Text) (K Text)) ns xs,
                   Generics.SOP.All FromJSON xs) 
               => AliasesFor ns
               -> Value
               -> Data.Aeson.Types.Parser r
recordFromJSON aliases value = 
    let aliases_xs :: NP (K Text) xs= hcoerce aliases
        parsers :: NP Parser2 xs = cpure_NP (Proxy @FromJSON) 
                                            (Parser2 (\fieldName o -> o .: fieldName))
        Parser1 gp = sequence_NP (liftA2_NP (\(K fieldName) (Parser2 f) -> Parser1 (f fieldName)) 
                                            aliases_xs
                                            parsers)
     in Generics.SOP.to . SOP . Z <$> withObject "Record" gp value

recordToJSON :: forall r xs ns.
                (IsProductType r xs, 
                 HasDatatypeInfo r,
                 FieldNamesOf r ~ ns,
                 AllZip (LiftedCoercible (K Text) (K Text)) ns xs,
                 Generics.SOP.All ToJSON xs) 
             => AliasesFor ns
             -> r
             -> Value
recordToJSON aliases r = 
    let aliases_xs :: NP (K Text) xs= hcoerce aliases
        rep = unZ (unSOP (Generics.SOP.from r))
        pairs = hcliftA2 (Proxy @ToJSON) (\(K a) (I x) -> K (a .= x)) aliases_xs rep
     in object (hcollapse pairs)

type family FieldNamesOf r :: [Symbol] where
    FieldNamesOf r = FieldNames (DatatypeInfoOf r)

type family FieldNames (a :: M.DatatypeInfo) :: [Symbol] where
    FieldNames ('M.ADT moduleName datatypeName '[M.Record constructorName fields]) = FieldNames' fields

type family FieldNames' (a :: [M.FieldInfo]) :: [Symbol] where
    FieldNames' '[] = '[]
    FieldNames' (('M.FieldInfo n) ': xs) = n ': FieldNames' xs

newtype Parser1 a = Parser1 { parseJSON1 :: Object -> Data.Aeson.Types.Parser a } deriving Functor

instance Applicative Parser1 where
    pure x = Parser1 (pure (pure x))
    Parser1 pa <*> Parser1 pb = Parser1 $ \v -> pa v <*> pb v 

newtype Parser2 a = Parser2 { parseJSON2 :: Text -> Object -> Data.Aeson.Types.Parser a } 

