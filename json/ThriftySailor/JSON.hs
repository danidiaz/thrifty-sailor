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
    ,   alias
    ,   FieldNamesOf
    ,   recordFromJSON
    ,   recordToJSON
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)            
import qualified Data.Text as Text
import           GHC.TypeLits (Symbol,KnownSymbol,symbolVal,TypeError)
import qualified GHC.TypeLits 
import qualified GHC.Generics as GHC
import           Generics.SOP
import           Generics.SOP.NP
import qualified Generics.SOP.Type.Metadata as M

type AliasesFor ns = NP (K Text) ns

alias :: forall (k :: Symbol). Text -> K Text k
alias = K

recordFromJSON :: forall r xs c cn ns. 
                  (IsProductType r xs, 
                   HasDatatypeInfo r,
                   ConstructorOf (DatatypeInfoOf r) ~ c,
                   ConstructorNameOf c ~ cn,
                   KnownSymbol cn,
                   ConstructorFieldNamesOf c ~ ns,
                   AllZip (LiftedCoercible (K Text) (K Text)) ns xs,
                   Generics.SOP.All FromJSON xs) 
               => AliasesFor ns
               -> Value
               -> Data.Aeson.Types.Parser r
recordFromJSON aliases value = 
    let aliases_xs :: NP (K Text) xs = 
            hcoerce aliases
        parsers :: NP Parser2 xs = 
            cpure_NP (Proxy @FromJSON) 
                     (Parser2 (\fieldName o -> o .: fieldName))
        applyFieldName (K fieldName) (Parser2 f) = 
            Parser1 (f fieldName)
        Parser1 gp = 
            sequence_NP (liftA2_NP applyFieldName aliases_xs parsers)
        constructorName = 
            symbolVal (Proxy @cn)
     in Generics.SOP.to . SOP . Z <$> withObject constructorName gp value

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
    let aliases_xs :: NP (K Text) xs = 
            hcoerce aliases
        rep = 
            unZ (unSOP (Generics.SOP.from r))
        pairs = 
            hcliftA2 (Proxy @ToJSON) (\(K a) (I x) -> K (a .= x)) aliases_xs rep
     in object (hcollapse pairs)

type family FieldNamesOf r :: [Symbol] where
    FieldNamesOf r = ConstructorFieldNamesOf (ConstructorOf (DatatypeInfoOf r))

type family ConstructorOf (a :: M.DatatypeInfo) :: M.ConstructorInfo where
    ConstructorOf ('M.ADT moduleName datatypeName '[constructor]) = constructor
    ConstructorOf t = 
        TypeError (GHC.TypeLits.Text "Sorry, this doesn't work for newtypes or "
                       GHC.TypeLits.:<>: 
                       GHC.TypeLits.Text "datatypes with multiple constructors."
                   GHC.TypeLits.:$$: 
                   GHC.TypeLits.Text "You tried to use it with: "
                   GHC.TypeLits.:$$: 
                   GHC.TypeLits.ShowType t)

type family ConstructorNameOf (a :: M.ConstructorInfo) :: Symbol where
    ConstructorNameOf ('M.Record constructorName fields) = constructorName
    ConstructorNameOf t =
        TypeError (GHC.TypeLits.Text "Sorry, this only works for plain records."
                   GHC.TypeLits.:$$: 
                   GHC.TypeLits.Text "You tried to use it with: "
                   GHC.TypeLits.:$$: 
                   GHC.TypeLits.ShowType t)

type family ConstructorFieldNamesOf (a :: M.ConstructorInfo) :: [Symbol] where
    ConstructorFieldNamesOf ('M.Record constructorName fields) = GetFieldNames fields

type family GetFieldNames (a :: [M.FieldInfo]) :: [Symbol] where
    GetFieldNames '[] = '[]
    GetFieldNames (('M.FieldInfo n) ': xs) = n ': GetFieldNames xs

newtype Parser1 a = Parser1 { parseJSON1 :: Object -> Data.Aeson.Types.Parser a } deriving Functor

instance Applicative Parser1 where
    pure x = Parser1 (pure (pure x))
    Parser1 pa <*> Parser1 pb = Parser1 $ \v -> pa v <*> pb v 

newtype Parser2 a = Parser2 { parseJSON2 :: Text -> Object -> Data.Aeson.Types.Parser a } 

