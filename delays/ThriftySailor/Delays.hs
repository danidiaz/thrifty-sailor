{-# LANGUAGE NumDecimals #-}
module ThriftySailor.Delays (
     Millis
   , millis
) where

import           Control.Concurrent
import           Streaming
import qualified Streaming.Prelude as S

newtype Millis = Millis { getMillis :: Int } deriving (Eq,Ord,Show)

millis :: Int -> Millis
millis = Millis


