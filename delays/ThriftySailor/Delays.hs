{-# LANGUAGE NumDecimals #-}
module ThriftySailor.Delays (
     Seconds
   , seconds
   , Factor
   , factor
   , giveUp
   , cutoff
) where

import           Control.Concurrent
import           Data.Time.Clock
import           Streaming
import qualified Streaming.Prelude as S

newtype Seconds = Seconds { getSeconds :: Int } deriving (Eq,Ord,Show)

seconds :: Int -> Seconds
seconds r = Seconds $ 
    if r < 1 
        then error $ "wrong delay range" ++ show r
        else r

newtype Factor = Factor { getFactor :: Double } deriving (Eq,Ord,Show)

factor :: Double -> Factor
factor r = Factor $
    if r < 1.0 || r > 3.0
        then error $ "wrong factor range" ++ show r
        else r

delays :: Seconds -> Factor -> Seconds -> Stream (Of ()) IO r
delays minDelay (Factor f) maxDelay = 
    do S.for (S.each values)
             (\t -> do liftIO (threadDelay t)
                       S.yield ())
       pure (error "should never reach here")
  where
    values = map ceiling
           . (++repeat (floatify maxDelay))
           . takeWhile (< floatify maxDelay) 
           $ iterate (*f) (floatify minDelay) 
    floatify = fromIntegral . (*1e6) . getSeconds

giveUp :: Seconds -> Stream (Of a) IO r -> Stream (Of a) IO (Either () r)  
giveUp = undefined

