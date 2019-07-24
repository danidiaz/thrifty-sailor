{-# LANGUAGE NumDecimals #-}
module Thrifty.Delays (
     Seconds
   , seconds
   , Factor
   , factor
   , waits
   , giveUp
   , retrying
   , complete
   , RetryPlan(..)
   , S.effects
) where

import           Control.Concurrent
import           Control.Monad
import           Data.Fixed
import           Data.Time.Clock
import           Streaming
import qualified Streaming.Prelude as S
import           Thrifty.Prelude
import           GHC.Stack

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

waits :: Seconds -- inital single wait length
      -> Factor -- multiplication factor
      -> Seconds -- maximum single wait lenght 
      -> Stream (Of ()) IO r
waits minDelay (Factor f) maxDelay = 
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
                
retrying :: Stream (Of ()) IO a -> IO (Either x a) -> Stream (Of x) IO a
retrying waits0 action = 
    S.zipWith (\_ x -> x) waits0 (S.untilRight action) 

giveUp :: Seconds -> Stream (Of a) IO r -> Stream (Of a) IO (Either () r)  
giveUp (Seconds s) stream = 
 do time0 <- lift getCurrentTime
    let tiredOfWaiting = 
              void  
            . S.untilRight
            $ do diff <- diffUTCTime time0 <$> getCurrentTime
                 pure $ if fromIntegral s < diff        
                           then Right ()
                           else Left ()
    S.zipWith (\_ a -> a) (Left <$> tiredOfWaiting) (Right <$> stream)

data RetryPlan = RetryPlan {
       giveUpAfter :: Seconds,
       initialDelay :: Seconds,
       increaseFactor :: Factor,
       maximumDelay :: Seconds
    }

complete :: Show a
         => RetryPlan
         -> (a -> Bool) -- ^ error check
         -> (a -> Bool) -- ^ completion check
         -> IO a 
         -> IO a
complete (RetryPlan giveUpAfter initialDelay increaseFactor maximumDelay) errCheck doneCheck action =
    do retries <- S.effects
                . giveUp giveUpAfter
                . retrying (waits initialDelay increaseFactor maximumDelay) 
                $ do a <- action
                     Thrifty.Prelude.log ("Checked again, and the result was: " ++ show a)
                     when (errCheck a) 
                          (throwIO (userError ("Action error.")))
                     pure $ if doneCheck a
                                then Right a
                                else Left ()
       liftError (const (userError ("Timeout waiting."))) retries

