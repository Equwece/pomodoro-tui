{-# LANGUAGE TemplateHaskell #-}

module Core (createApp, initialAppState, PomodoroEvent (..)) where

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
import Brick.Widgets.List (GenericList (listSelected), handleListEvent, handleListEventVi, list, renderList)
import Control.Lens
import Data.Vector (Vector, fromList)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)

data AppState = AppState
  { _workTime :: Int,
    _restTime :: Int,
    _longRestTime :: Maybe Int,
    _currentPomodoroState :: PomodoroState,
    _currentTimer :: Int,
    _currentControlList :: GenericList String Vector String
  }
  deriving (Show)

data PomodoroState = Work | Rest | LongRest | Pause PomodoroState deriving (Eq, Show)

makeLenses ''AppState

controlList :: GenericList String Vector String
controlList = list "ControlList" (fromList ["Stop", "Next"]) 1

renderControlListItem :: Bool -> String -> Widget String
renderControlListItem isSelected button = str (button <> selectMark)
  where
    selectMark = if isSelected then " *" else ""

drawUI :: AppState -> [Widget String]
drawUI currentState =
  [ str (renderCurrentTimer $ currentState ^. currentTimer)
      <=> str (show $ currentState ^. currentPomodoroState)
      <=> renderList renderControlListItem True (currentState ^. currentControlList)
  ]

renderCurrentTimer :: Int -> String
renderCurrentTimer currentTime = minutes <> ":" <> seconds
  where
    minutes = show $ div currentTime 60
    seconds = show $ mod currentTime 60

handleEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleEvent currentState (AppEvent Second) = do
  continue $ handleSecond currentState
handleEvent currentState be@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleControlListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleControlListEvents currentState ve
  (V.EvKey V.KEnter []) -> continue $ handleControlListSelect currentState
  (V.EvKey (V.KChar 'q') []) -> resizeOrQuit currentState be
  _ -> continue currentState
handleEvent currentState _ = continue currentState

handleControlListSelect :: AppState -> AppState
handleControlListSelect currentState = case selectedElement of
  Just 0 -> case currentState ^. currentPomodoroState of
    Pause (Pause _) -> currentState
    Pause a -> currentState & currentPomodoroState .~ a
    b -> currentState & currentPomodoroState .~ Pause b
  Just 1 -> case currentState ^. currentPomodoroState of
    Pause Work -> setPomodoroRest currentState
    Pause Rest -> setPomodoroWork currentState
    Work -> setPomodoroRest currentState
    Rest -> setPomodoroWork currentState
    _ -> currentState
  _ -> currentState
  where
    selectedElement = listSelected (currentState ^. currentControlList)

handleControlListEvents :: AppState -> V.Event -> EventM String (Next AppState)
handleControlListEvents currentState ve = do
  newControlList <- handleListEventVi handleListEvent ve controlList
  continue (currentState & currentControlList .~ newControlList)

handleSecond :: AppState -> AppState
handleSecond currentState = case currentState ^. currentPomodoroState of
  Pause _ -> currentState
  _ -> case currentState ^. currentTimer of
    0 -> decreasePomodoroTimer . decideNewPomodoroState $ currentState
    _ -> decreasePomodoroTimer currentState

decideNewPomodoroState :: AppState -> AppState
decideNewPomodoroState currentState = case currentState ^. currentPomodoroState of
  Work -> setPomodoroRest currentState
  _ -> setPomodoroWork currentState

setPomodoroRest :: AppState -> AppState
setPomodoroRest currentState = currentState & currentPomodoroState .~ Rest & currentTimer .~ (currentState ^. restTime)

setPomodoroWork :: AppState -> AppState
setPomodoroWork currentState = currentState & currentPomodoroState .~ Work & currentTimer .~ (currentState ^. workTime)

decreasePomodoroTimer :: AppState -> AppState
decreasePomodoroTimer currentState = currentState & currentTimer -~ 1

data PomodoroEvent = Second

createApp :: App AppState PomodoroEvent String
createApp =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appStartEvent = \_ -> return initialAppState,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

initialAppState :: AppState
initialAppState =
  AppState
    { _workTime = 1500,
      _restTime = 300,
      _longRestTime = Just 900,
      _currentPomodoroState = Work,
      _currentTimer = 1500,
      _currentControlList = controlList
    }
