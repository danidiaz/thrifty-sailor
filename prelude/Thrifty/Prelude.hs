{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Thrifty.Prelude (
        LiftError (..)
    ,   errorShow
    ,   ZeroMoreThanOne(..)
    ,   uniqueness
    ,   absence
    ,   log
    ,   doable
    ,   IOException
    ,   userError
    ,   throwIO
    ) where

import           Prelude hiding (log)
import           Control.Monad.Except 
import           Control.Monad.IO.Class
import           Control.Exception
import           Data.Bifunctor
import           Data.Foldable
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           System.IO
import           GHC.Stack

class LiftError (k :: Type -> Type) (e :: Type) where
    type Adapter k e :: Type
    liftError :: MonadError e m => Adapter k e -> k r -> m r 

instance LiftError (Either e') e where
    type Adapter (Either e') e = e' -> e
    liftError f = either throwError return . first f

instance LiftError Maybe e where
    type Adapter Maybe e = e
    liftError e = maybe (throwError e) return

errorShow :: Show x => x -> IOError
errorShow = userError . show

data ZeroMoreThanOne a = Zero
                       | MoreThanOne a a [a]
                       deriving (Show,Eq)

uniqueness :: Foldable f => f a -> Either (ZeroMoreThanOne a) a
uniqueness container = case Data.Foldable.toList container of
    []          -> Left $ Zero
    a : []      -> Right $ a
    a : a' : as -> Left $ MoreThanOne a a' as

absence :: Foldable f => f a -> Either (NonEmpty a) ()
absence container = case Data.Foldable.toList container of
    []          -> Right $ ()
    a : as      -> Left $ a :| as

-- | Emit message on stderr
log :: MonadIO m => String -> m ()
log = liftIO . hPutStrLn stderr

doable :: (Show target,Show source,MonadError IOException m,MonadIO m,HasCallStack)
       => m [target]
       -> (target -> Bool)
       -> m [source]
       -> (source -> Bool)
       -> m source
doable listTargets checkTarget listSources checkSource =
    do log "Checking that target doesn't already exist..."
       ts <- listTargets
       liftError errorShow (absence (filter checkTarget ts))
       log "Checking that the source exists..."
       ss <- listSources
       s <- liftError errorShow (uniqueness (filter checkSource ss))
       pure s

