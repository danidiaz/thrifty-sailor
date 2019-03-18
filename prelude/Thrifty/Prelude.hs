{-# language RankNTypes, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Thrifty.Prelude (
        LiftError (..)
    ,   errorShow
    ,   ZeroMoreThanOne(..)
    ,   unique
    ,   absent
    ,   log
    ) where

import           Prelude hiding (log)
import           Control.Monad.Except 
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty
import           System.IO
import           Data.Kind

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

unique :: (MonadError e' m, Foldable f) => (ZeroMoreThanOne a -> e') -> f a -> m a 
unique errFunc container = case Data.Foldable.toList container of
    [] -> throwError (errFunc Zero)
    a : [] -> return a
    a : a' : as -> throwError (errFunc (MoreThanOne a a' as))

absent :: (MonadError e' m, Foldable f) => (NonEmpty a -> e') -> f a -> m ()
absent errFunc container = case Data.Foldable.toList container of
    [] -> return ()
    a : as -> throwError (errFunc (a :| as))

-- | Emit message on stderr
log :: String -> IO ()
log = hPutStrLn stderr  
