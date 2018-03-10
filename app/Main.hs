{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      (returnA, (<<<), (>>>))
import           Control.Concurrent (forkIO, threadDelay)
import           Data.Flux
import           Data.List          (delete)
import           Data.Maybe         (maybe)
import           SDL

data Direction = L
               | R
               | U
               | D
  deriving (Show, Eq)

data Cmd = StartMoving
         | StopMoving
         | ChangeDirection Direction
  deriving (Show)

newtype InputManager = InputManager [Direction]

defaultInputManager = InputManager []

emptyManager :: InputManager -> Bool
emptyManager (InputManager l) = null l

topDirection :: InputManager -> Maybe Direction
topDirection (InputManager (x:_)) = Just x
topDirection _                    = Nothing

addDirection :: Direction -> InputManager -> InputManager
addDirection dir (InputManager l) =
  if dir `elem` l
  then InputManager l
  else InputManager (dir:l)

removeDirection :: Direction -> InputManager -> InputManager
removeDirection dir (InputManager l) =
  InputManager (dir `delete` l)

keyboardEventToDirection :: KeyboardEventData -> Maybe Direction
keyboardEventToDirection kev =
  let keycode = keysymKeycode <<< keyboardEventKeysym $ kev in
    case keycode of
      KeycodeUp    -> Just U
      KeycodeRight -> Just R
      KeycodeLeft  -> Just L
      KeycodeDown  -> Just D
      _            -> Nothing

processKeyboardEvent :: KeyboardEventData -> InputManager -> InputManager
processKeyboardEvent kev man =
  case keyboardEventKeyMotion kev of
    Pressed -> maybe man (`addDirection` man) (keyboardEventToDirection kev)
    Released -> maybe man (`removeDirection` man) (keyboardEventToDirection kev)

inputManagerCmd :: InputManager -> InputManager -> [Cmd]
inputManagerCmd old new =
  [StartMoving | emptyManager old && not (emptyManager new)]
  ++ [StopMoving | not (emptyManager old) && emptyManager new]
  ++ (if topDirection old /= topDirection new
      then maybe [] (pure . ChangeDirection) (topDirection new)
      else [])

keyboard :: InputManager -> Producer Cmd
keyboard man = Producer $ do
  ev <- waitEvent
  let man' = case eventPayload ev of
               KeyboardEvent kev -> processKeyboardEvent kev man
               _                 -> man

  pure (inputManagerCmd man man', keyboard man')

logger :: Consumer Cmd
logger = Consumer $ \cmd -> do
  print cmd
  pure logger

main :: IO ()
main = do
  initializeAll
  window   <- createWindow "Data.Flux test application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  k <- source $ keyboard defaultInputManager
  l <- sink logger

  forever (input k >>> output l)
