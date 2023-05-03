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
drawUI currentState =
  [ str (renderCurrentTimer $ currentState ^. currentTimer)
      <=> str (show $ currentState ^. currentPomodoroState)
  ]

renderCurrentTimer :: Int -> String
renderCurrentTimer currentTime = minutes <> ":" <> seconds
  where
    minutes = show $ div currentTime 60
    seconds = show $ mod currentTime 60

handleEvent :: AppState -> BrickEvent n PomodoroEvent -> EventM n (Next AppState)
handleEvent currentState (AppEvent Second) = do
  continue $ getNewState currentState
handleEvent currentState be = resizeOrQuit currentState be

getNewState :: AppState -> AppState
getNewState currentState = case currentState ^. currentTimer of
  0 -> decreasePomodoroTimer . decideNewPomodoroState $ currentState
  _ -> decreasePomodoroTimer currentState

decideNewPomodoroState :: AppState -> AppState
decideNewPomodoroState currentState = case currentState ^. currentPomodoroState of
  Work -> currentState & currentPomodoroState .~ Rest & currentTimer .~ (currentState ^. restTime)
  _ -> currentState & currentPomodoroState .~ Work & currentTimer .~ (currentState ^. workTime)

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
