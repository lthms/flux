{-# LANGUAGE Arrows #-}

module Control.Arrow.Flux.Time
  ( hold
  , now
  , diffTime
  , tick
  ) where

import           Control.Arrow
import           Control.Arrow.Flux
import           Control.Category
import           Control.Concurrent
import           Data.Maybe         (fromMaybe)
import           Data.Time.Clock
import           Prelude            hiding (id)

tick :: Int -- ^ Microseconds
     -> a   -- ^ The value to send
     -> IO (Source a)
tick n x = source step
  where step = Producer $ do
          threadDelay n
          pure ([x], step)

hold :: Int -- ^ Microseconds to hold the inputs
     -> Flux a a
hold n = Flux $ \x -> threadDelay n >> pure (x, hold n)

-- | Ignore the input and returns the current 'UTCTime'.
now :: Flux a UTCTime
now = Flux $ \_ -> do
  x <- getCurrentTime
  pure (x, now)

-- | Ignore the input, and returns the 'DiffTime' since its last input.
--   treat. The first time, '0' is returned.
diffTime :: Flux a NominalDiffTime
diffTime = proc _ -> do
  rec
    b <- delay Nothing -< st
    n <- now -< ()
    st <- id -< Just n

  returnA -< diffUTCTime n (fromMaybe n b)
