{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      ((&&&), (<<<), (>>>))
import           Control.Arrow.Flux
import           Data.List          (delete)
import           Data.Maybe         (maybe)
import           Data.Text          (Text)
import           Data.Time.Clock    (getCurrentTime)
import           Network.Socket     (withSocketsDo)
import qualified Network.WebSockets as WS
import           SDL

data Direction = L
               | R
               | U
               | D
  deriving (Show, Eq)

data Cmd = StartMoving
         | StopMoving
         | ChangeDirection Direction
         | ServerNotification Text
  deriving (Show)

newtype InputManager = InputManager [Direction]

defaultInputManager :: InputManager
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

fromServer :: WS.Connection -> Producer Cmd
fromServer ws = Producer $ do
  msg <- WS.receiveData ws
  pure ([ServerNotification msg], fromServer ws)

toServer :: WS.Connection -> Consumer Cmd
toServer ws = Consumer $ \x -> do
  case x of
    ChangeDirection U -> WS.sendTextData ws ("UP" :: Text)
    ChangeDirection D -> WS.sendTextData ws ("DOWN" :: Text)
    ChangeDirection L -> WS.sendTextData ws ("LEFT" :: Text)
    ChangeDirection R -> WS.sendTextData ws ("RIGHT" :: Text)
    StartMoving       -> WS.sendTextData ws ("MOVE" :: Text)
    StopMoving        -> WS.sendTextData ws ("STOP" :: Text)
    _                 -> pure ()
  pure (toServer ws)

logger :: Consumer Cmd
logger = Consumer $ \cmd -> do
  now <- getCurrentTime
  putStrLn $ "[" ++ show now ++ "]: " ++ show cmd
  pure logger

main :: IO ()
main = withSocketsDo $ WS.runClient "demo.lkn.ist" 80 "/ws" $ \ws -> do
  initializeAll
  _ <- createWindow "Data.Flux test application" defaultWindow

  fs <- source $ fromServer ws
  k <- source $ keyboard defaultInputManager
  l <- sink logger
  ts <- sink $ toServer ws

  forever (select [k, fs] >>> (output l &&& output ts))
