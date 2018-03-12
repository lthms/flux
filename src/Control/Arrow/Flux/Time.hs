{-# LANGUAGE Arrows #-}

module Control.Arrow.Flux.Time
  ( hold
  , currentTime
  , elapseTime
  , tick
  ) where

import           Control.Arrow
import           Control.Arrow.Flux
import           Control.Category
import           Control.Concurrent
import           Data.Maybe         (fromMaybe)
import           Data.Time.Clock

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
currentTime :: Flux a UTCTime
currentTime = Flux $ \_ -> do
  x <- getCurrentTime
  pure (x, currentTime)

-- | Ignore the input, and returns the 'DiffTime' since its last input.
--   treat. The first time, '0' is returned.
elapseTime :: Flux a NominalDiffTime
elapseTime = proc _ -> do
  rec
    before <- delay Nothing -< mnow
    now <- currentTime -< ()
    let mnow = Just now

  returnA -< diffUTCTime now (fromMaybe now before)
