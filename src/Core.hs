{-# LANGUAGE TemplateHaskell #-}

module Core where

import Brick
  ( App (..),
    BrickEvent (..),
    EventM (..),
    Next,
    Widget,
    attrMap,
    continue,
    neverShowCursor,
    resizeOrQuit,
    str,
    (<=>),
  )
import Control.Lens
import Graphics.Vty.Attributes (defAttr)

data AppState = AppState
  { _workTime :: Int,
    _restTime :: Int,
    _longRestTime :: Maybe Int,
    _currentPomodoroState :: PomodoroState,
    _currentTimer :: Int
  }
  deriving (Eq, Show)

data PomodoroState = Work | Rest | LongRest deriving (Eq, Show)

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI currentState = [str (show $ currentState ^. currentTimer) <=> str (show $ currentState ^. currentPomodoroState)]

handleEvent :: AppState -> BrickEvent n PomodoroEvent -> EventM n (Next AppState)
handleEvent currentState (AppEvent Second) = do
  continue $ decreasePomodoroTimer currentState
handleEvent currentState be = resizeOrQuit currentState be

decreasePomodoroTimer :: AppState -> AppState
decreasePomodoroTimer currentState = currentState & currentTimer -~ 1

data PomodoroEvent = Second

createApp :: App AppState PomodoroEvent ()
createApp =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appStartEvent = \_ -> return initalAppState,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

initalAppState :: AppState
initalAppState =
  AppState
    { _workTime = 1500,
      _restTime = 300,
      _longRestTime = Just 900,
      _currentPomodoroState = Work,
      _currentTimer = 1500
    }
