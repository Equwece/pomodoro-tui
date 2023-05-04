{-# LANGUAGE TemplateHaskell #-}

module Core (createApp, initialAppState, PomodoroEvent (..)) where

import Brick
  ( App (..),
    BrickEvent (..),
    EventM (..),
    Next,
    Padding (Pad),
    Widget,
    attrMap,
    continue,
    hLimit,
    neverShowCursor,
    padBottom,
    padTop,
    resizeOrQuit,
    str,
    vBox,
    vLimit,
    withBorderStyle,
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.List (GenericList (listSelected), handleListEvent, handleListEventVi, list, renderList)
import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Vector (Vector, fromList)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)
import Utils (sendNotification)

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

makeControlList :: PomodoroState -> GenericList String Vector String
makeControlList pomodoroState = list "ControlList" (fromList [toggleStopLabel, "Next"]) 1
  where
    toggleStopLabel = case pomodoroState of
      Pause _ -> "Continue"
      _ -> "Stop"

renderControlListItem :: Bool -> String -> Widget String
renderControlListItem isSelected button = hCenter $ str (leftMark <> button <> rightMark)
  where
    (leftMark, rightMark) = if isSelected then ("< ", " >") else ("", "")

drawUI :: AppState -> [Widget String]
drawUI currentState =
  [ vBox
      [ center . withBorderStyle unicodeRounded . border $
          vBox . map (hLimit 20 . hCenter) $
            [ padTop (Pad 2) $ str (renderCurrentTimer $ currentState ^. currentTimer),
              padBottom (Pad 2) $ str (show $ currentState ^. currentPomodoroState)
            ],
        vLimit 15 $ renderList renderControlListItem True (currentState ^. currentControlList)
      ]
  ]

renderCurrentTimer :: Int -> String
renderCurrentTimer currentTime = minutes <> ":" <> seconds
  where
    minutes = renderTime . show $ div currentTime 60
    seconds = renderTime . show $ mod currentTime 60
    renderTime t = case t of
      [_] -> '0' : t
      _ -> t

handleEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleEvent currentState (AppEvent Second) = handleSecond currentState
handleEvent currentState be@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleControlListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleControlListEvents currentState ve
  (V.EvKey V.KUp []) -> handleControlListEvents currentState ve
  (V.EvKey V.KDown []) -> handleControlListEvents currentState ve
  (V.EvKey V.KEnter []) -> handleControlListSelect currentState
  (V.EvKey (V.KChar ' ') []) -> handleControlListSelect currentState
  (V.EvKey (V.KChar 'q') []) -> resizeOrQuit currentState be
  _ -> continue currentState
handleEvent currentState _ = continue currentState

handleControlListSelect :: AppState -> EventM String (Next AppState)
handleControlListSelect currentState = case selectedElement of
  -- "Stop" button is pressed
  Just 0 -> case currentState ^. currentPomodoroState of
    Pause (Pause _) -> continue currentState
    Pause a -> continue $ currentState & currentPomodoroState .~ a & currentControlList .~ makeControlList a
    b -> continue $ currentState & currentPomodoroState .~ Pause b & currentControlList .~ makeControlList (Pause b)
  -- "Next" button is pressed
  Just 1 -> case currentState ^. currentPomodoroState of
    Pause Work -> setPomodoroRest currentState >>= continue
    Pause Rest -> setPomodoroWork currentState >>= continue
    Work -> setPomodoroRest currentState >>= continue
    Rest -> setPomodoroWork currentState >>= continue
    _ -> continue currentState
  _ -> continue currentState
  where
    selectedElement = listSelected (currentState ^. currentControlList)

handleControlListEvents :: AppState -> V.Event -> EventM String (Next AppState)
handleControlListEvents currentState ve = do
  newControlList <- handleListEventVi handleListEvent ve (makeControlList (currentState ^. currentPomodoroState))
  continue (currentState & currentControlList .~ newControlList)

handleSecond :: AppState -> EventM String (Next AppState)
handleSecond currentState = case currentState ^. currentPomodoroState of
  Pause _ -> continue currentState
  _ -> case currentState ^. currentTimer of
    0 -> do
      newPomodoroState <- decideNewPomodoroState currentState
      decreasePomodoroTimer newPomodoroState
    _ -> decreasePomodoroTimer currentState

decideNewPomodoroState :: AppState -> EventM String AppState
decideNewPomodoroState currentState = case currentState ^. currentPomodoroState of
  Work -> setPomodoroRest currentState
  _ -> setPomodoroWork currentState

setPomodoroRest :: AppState -> EventM String AppState
setPomodoroRest currentState = do
  _ <- liftIO $ sendNotification ("Time to Rest: " <> renderCurrentTimer (currentState ^. restTime))
  return $ currentState & currentPomodoroState .~ Rest & currentTimer .~ currentState ^. restTime

setPomodoroWork :: AppState -> EventM String AppState
setPomodoroWork currentState = do
  _ <- liftIO $ sendNotification ("Time to Work: " <> renderCurrentTimer (currentState ^. workTime))
  return $ currentState & currentPomodoroState .~ Work & currentTimer .~ currentState ^. workTime

decreasePomodoroTimer :: AppState -> EventM String (Next AppState)
decreasePomodoroTimer currentState = continue $ currentState & currentTimer -~ 1

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
      _currentControlList = makeControlList Work
    }
