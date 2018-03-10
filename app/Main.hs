{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      (returnA, (<<<), (>>>))
import           Control.Concurrent (forkIO, threadDelay)
import           Data.Flux
import           SDL

data Cmd = Keyboard
  deriving (Show)

keyboard :: Producer Cmd
keyboard = Producer $ do
  waitEvent
  pure (Keyboard, keyboard)

logger :: Consumer Cmd
logger = Consumer $ \cmd -> do
  print cmd
  pure logger

main :: IO ()
main = do
  initializeAll
  window   <- createWindow "Data.Flux test application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  k <- source keyboard
  l <- sink logger

  forever (input k >>> output l)
