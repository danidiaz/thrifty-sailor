module ThriftySailor.Prelude (
        eitherError
    ,   maybeError
    ) where

import Control.Monad.Except 
import Data.Bifunctor

eitherError :: MonadError e' m => (e -> e') -> Either e r -> m r 
eitherError f = either throwError return . first f

maybeError :: MonadError e' m => e' -> Maybe r -> m r 
maybeError e' = maybe (throwError e') return
