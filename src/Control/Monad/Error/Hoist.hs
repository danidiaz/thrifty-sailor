{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Copied wholesale from Jon Sterling's hoist-error, slightly adapted to use ExceptT.
--   http://hackage.haskell.org/package/hoist-error-0.1.0.2

module Control.Monad.Error.Hoist
( HoistError(..)
, (<%?>)
, haulError
, (<%!?>)
, joinError
, (<?>)
, haulError_
, (<!?>)
, joinError_
) where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans

-- | A tricky class for easily hoisting errors out of partiality types (e.g.
-- 'Maybe', @'Either' e@) into a monad. The parameter @e@ represents the error
-- information carried by the partiality type @t@, and @e'@ represents the type
-- of error expected in the monad @m@.
--
class Monad m => HoistError m t e e' | t -> e where

  -- | Given a conversion from the error in @t α@ to @e'@, we can hoist the
  -- computation into @m@.
  --
  hoistError
    :: (e -> e')
    -> t a
    -> m a

instance MonadError e m => HoistError m Maybe () e where
  hoistError f = maybe (throwError $ f ()) return

instance MonadError e' m => HoistError m (Either e) e e' where
  hoistError f = either (throwError . f) return

instance (m ~ n, MonadError e' m) => HoistError m (ExceptT e n) e e' where
  hoistError f e = do
    bare <- runExceptT e
    case bare of
        Left err -> throwError . f $ err
        Right value -> return value

-- | A flipped synonym for 'hoistError'.
(<%?>)
  :: HoistError m t e e'
  => t a
  -> (e -> e')
  -> m a
(<%?>) = flip hoistError

haulError
  :: HoistError m t e e'
  => t a
  -> (e -> e')
  -> m a
haulError = (<%?>)

infixl 8 <%?>
{-# INLINE (<%?>) #-}

-- | A version of '<%?>' that operates on values already in the monad.
--
(<%!?>)
  :: HoistError m t e e'
  => m (t a)
  -> (e -> e')
  -> m a
m <%!?> e = do
  x <- m
  x <%?> e

joinError
  :: HoistError m t e e'
  => m (t a)
  -> (e -> e')
  -> m a
joinError = (<%!?>)

infixl 8 <%!?>
{-# INLINE (<%!?>) #-}

-- | A version of @hoistError@ that ignores the error in @t α@ and replaces it
-- with a new one in @e'@.
--
(<?>)
  :: HoistError m t e e'
  => t a
  -> e'
  -> m a
m <?> e = m <%?> const e

haulError_
  :: HoistError m t e e'
  => t a
  -> e'
  -> m a
haulError_ = (<?>)

infixl 8 <?>
{-# INLINE (<?>) #-}

-- | A version of @<?>@ that operates on values already in the monad.
(<!?>)
  :: HoistError m t e e'
  => m (t a)
  -> e'
  -> m a
m <!?> e = do
  x <- m
  x <?> e

joinError_
  :: HoistError m t e e'
  => m (t a)
  -> e'
  -> m a
joinError_ = (<!?>)

infixl 8 <!?>
{-# INLINE (<!?>) #-}
