{-# language RankNTypes #-}
module ThriftySailor.Prelude (
        eitherError
    ,   maybeError
    ,   ZeroMoreThanOne(..)
    ,   unique
    ,   absent
    ) where

import Control.Monad.Except 
import Data.Bifunctor
import Data.Foldable
import Data.List.NonEmpty

eitherError :: MonadError e' m => (e -> e') -> Either e r -> m r 
eitherError f = either throwError return . first f

maybeError :: MonadError e' m => e' -> Maybe r -> m r 
maybeError e' = maybe (throwError e') return

data ZeroMoreThanOne a = Zero
                       | MoreThanOne a a [a]
                       deriving (Show,Eq)

unique :: (MonadError e' m, Foldable f) => (a -> Bool) -> (ZeroMoreThanOne a -> e') -> f a -> m a 
unique predicate errFunc container = case Prelude.filter predicate (Data.Foldable.toList container) of
    [] -> throwError (errFunc Zero)
    a : [] -> return a
    a : a' : as -> throwError (errFunc (MoreThanOne a a' as))

absent :: (MonadError e' m, Foldable f) => (a -> Bool) -> (NonEmpty a -> e') -> f a -> m ()
absent predicate errFunc container = case Prelude.filter predicate (Data.Foldable.toList container) of
    [] -> return ()
    a : as -> throwError (errFunc (a :| as))
