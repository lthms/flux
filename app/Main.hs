{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      (returnA, (&&&), (<<<), (>>>))
import           Control.Arrow.Flux
import           Data.List          (delete)
import           Data.Maybe         (maybe)
import           Data.Text          (Text)
import           Data.Time.Clock    (getCurrentTime)
import           Network.Socket     (withSocketsDo)
import qualified Network.WebSockets as WS
import           SDL                hiding (delay)

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

currentDirection :: InputManager -> Maybe Direction
currentDirection (InputManager (x:_)) = Just x
currentDirection _                    = Nothing

addDirection :: Direction -> InputManager -> InputManager
addDirection dir (InputManager l) =
  if dir `elem` l
  then InputManager l
  else InputManager (dir:l)

removeDirection :: Direction -> InputManager -> InputManager
removeDirection dir (InputManager l) = InputManager (dir `delete` l)

keyboardEventToDirection :: KeyboardEventData -> Maybe Direction
keyboardEventToDirection = keyboardEventKeysym >>> keysymKeycode >>> toDir
  where toDir :: Keycode -> Maybe Direction
        toDir KeycodeUp    = Just U
        toDir KeycodeRight = Just R
        toDir KeycodeLeft  = Just L
        toDir KeycodeDown  = Just D
        toDir _            = Nothing

processKeyboardEvent :: KeyboardEventData -> InputManager -> InputManager
processKeyboardEvent kev man =
  case keyboardEventKeyMotion kev of
    Pressed  -> maybe man (`addDirection` man) (keyboardEventToDirection kev)
    Released -> maybe man (`removeDirection` man) (keyboardEventToDirection kev)

inputManagerCmd :: InputManager -> InputManager -> [Cmd]
inputManagerCmd old new =
  [StartMoving | emptyManager old && not (emptyManager new)]
  ++ [StopMoving | not (emptyManager old) && emptyManager new]
  ++ (if currentDirection old /= currentDirection new
      then maybe [] (pure . ChangeDirection) (currentDirection new)
      else [])

fromKeyboard :: Producer Cmd
fromKeyboard = enumP (keyboard *->> inputManager)
  where
    keyboard :: Producer (Maybe KeyboardEventData)
    keyboard = repeatP $ do
      ev <- waitEvent
      case eventPayload ev of
        KeyboardEvent kev -> pure (Just kev)
        _                 -> pure Nothing

    inputManager :: Flux KeyboardEventData [Cmd]
    inputManager = proc kev -> do
      rec
        man <- delay defaultInputManager -< man'
        let man' = processKeyboardEvent kev man

      returnA -< inputManagerCmd man man'

fromServer :: WS.Connection -> Producer Cmd
fromServer ws = repeatP $ ServerNotification <$> WS.receiveData ws

toServer :: WS.Connection -> Consumer Cmd
toServer ws = repeatC $ \case
    ChangeDirection U -> WS.sendTextData ws ("UP" :: Text)
    ChangeDirection D -> WS.sendTextData ws ("DOWN" :: Text)
    ChangeDirection L -> WS.sendTextData ws ("LEFT" :: Text)
    ChangeDirection R -> WS.sendTextData ws ("RIGHT" :: Text)
    StartMoving       -> WS.sendTextData ws ("MOVE" :: Text)
    StopMoving        -> WS.sendTextData ws ("STOP" :: Text)
    _                 -> pure ()

logger :: Consumer Cmd
logger = repeatC $ \cmd -> do
  now <- getCurrentTime
  putStrLn $ "[" ++ show now ++ "]: " ++ show cmd

main :: IO ()
main = withSocketsDo $ WS.runClient "demo.lkn.ist" 80 "/ws" $ \ws -> do
  initializeAll
  _ <- createWindow "Data.Flux test application" defaultWindow

  fs <- source $ fromServer ws -- one task to read what comes through the
                               -- websocket
  k <- source fromKeyboard -- one task to read the keyboard events
  l <- sink logger -- one task to log every command generated by the two
                   -- previous tasks
  ts <- sink $ toServer ws -- one task to send back to the server a subset of
                           -- those commands

  -- wait for either the server or the keyboard to generate some inputs, then
  -- send the generated commands to both the logger and the server
  forever (select [k, fs] >>> (output l &&& output ts))
