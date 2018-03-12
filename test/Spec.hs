{-# LANGUAGE Arrows #-}

module Main where

import           Control.Arrow
import           Control.Arrow.Flux
import           Control.Concurrent
import           Control.Monad      (when)
import           Data.Time.Clock    (NominalDiffTime, diffUTCTime,
                                     getCurrentTime)

wait :: Int -> Producer ()
wait n = Producer $ do
  threadDelay (n * 1000000)
  pure ((), wait n)

delay' :: Int -> Flux a a
delay' n = Flux $ \x -> threadDelay (n * 1000000) >> pure (x, delay' n)

chrono :: IO a
       -> IO NominalDiffTime
chrono x = do
  before <- getCurrentTime
  _ <- x
  after <- getCurrentTime
  pure (diffUTCTime after before)

main :: IO ()
main = do
  dt1 <- chrono $ once () (h <$> delay' 2 <*> delay' 2)
  dt2 <- chrono $ once () (proc i -> do
                              (x, y) <- (delay' 2 &&& delay' 2) -< i
                              returnA -< h x y)

  when (abs (dt2 - 2.0) > 0.1) (error "we have waited too long for the first test")
  when (abs (dt1 - 2.0) > 0.1) (error "we have waited too long for the second test")
  where h _ _ = ()
