{-# LANGUAGE DuplicateRecordFields #-}

module Core (createApp, initialAppState, setAppProfile, buildInitState) where

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
import Control.Lens ((&), (-~), (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.Vector (Vector, fromList)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)
import Types
  ( AppState (..),
    Page (Main, Profiles),
    PomodoroEvent (..),
    PomodoroState (Pause, Rest, Work),
    Profile (..),
    ProfileContainer (ProfileContainer, currentProfileId, profiles),
    appPage,
    appProfiles,
    currentControlList,
    currentPomodoroState,
    currentProfile,
    currentProfileList,
    currentTimer,
    defaultAppProfile,
    profileId,
    restTime,
    workTime,
  )
import Utils (saveProfileConfig, sendNotification)

makeControlList :: PomodoroState -> GenericList String Vector String
makeControlList pomodoroState = list "ControlList" (fromList [toggleStopLabel, "Next", "Profiles"]) 1
  where
    toggleStopLabel = case pomodoroState of
      Pause _ -> "Continue"
      _ -> "Stop"

makeProfileList :: Map UUID Profile -> GenericList String Vector String
makeProfileList state = list "ControlList" (fromList (map renderProfileItem $ getProfileListFromMap state)) 1

renderProfileItem :: Profile -> String
renderProfileItem profile =
  "Profile "
    <> renderCurrentTimer (profile ^. workTime)
    <> "/"
    <> renderCurrentTimer (profile ^. restTime)

getProfileListFromMap :: Map UUID Profile -> [Profile]
getProfileListFromMap = map snd . M.toList

renderControlListItem :: Bool -> String -> Widget String
renderControlListItem isSelected button = hCenter $ str (leftMark <> button <> rightMark)
  where
    (leftMark, rightMark) = if isSelected then ("< ", " >") else ("", "")

renderProfileListItem :: Bool -> String -> Widget String
renderProfileListItem isSelected button = hCenter . padBottom (Pad 1) $ str (leftMark <> button <> rightMark)
  where
    (leftMark, rightMark) = if isSelected then ("< ", " >") else ("", "")

drawUI :: AppState -> [Widget String]
drawUI currentState = case currentState ^. appPage of
  Profiles -> renderProfilePage currentState
  _ -> renderMainPage currentState

renderMainPage :: AppState -> [Widget String]
renderMainPage currentState =
  [ vBox
      [ center . withBorderStyle unicodeRounded . border $
          vBox . map (hLimit 20 . hCenter) $
            [ padTop (Pad 2) $ str (renderCurrentTimer $ currentState ^. currentTimer),
              padBottom (Pad 2) $ str (show $ currentState ^. currentPomodoroState)
            ],
        vLimit 15 $ renderList renderProfileListItem True (currentState ^. currentControlList)
      ]
  ]

renderProfilePage :: AppState -> [Widget String]
renderProfilePage currentState =
  [ center $
      vBox
        [ vLimit 15 $ renderList renderProfileListItem True (currentState ^. currentProfileList)
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
handleEvent currentState event = case currentState ^. appPage of
  Profiles -> handleProfilePageEvent currentState event
  Main -> handleMainPageEvent currentState event
  _ -> handleGenericPageEvent currentState event

handleMainPageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleMainPageEvent currentState (AppEvent Second) = handleSecond currentState
handleMainPageEvent currentState event@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleControlListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleControlListEvents currentState ve
  (V.EvKey V.KUp []) -> handleControlListEvents currentState ve
  (V.EvKey V.KDown []) -> handleControlListEvents currentState ve
  (V.EvKey V.KEnter []) -> handleControlListSelect currentState
  (V.EvKey (V.KChar ' ') []) -> handleControlListSelect currentState
  _ -> do
    handleGenericPageEvent currentState event
handleMainPageEvent currentState event = handleGenericPageEvent currentState event

handleProfilePageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleProfilePageEvent currentState (AppEvent Second) = handleSecond currentState
handleProfilePageEvent currentState event@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleProfileListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KUp []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KDown []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KEnter []) -> handleProfileListSelect currentState
  (V.EvKey (V.KChar ' ') []) -> handleProfileListSelect currentState
  (V.EvKey V.KEsc []) -> continue (currentState & appPage .~ Main)
  (V.EvKey (V.KChar 'd') []) -> handleProfileListDelete currentState
  _ -> do
    handleGenericPageEvent currentState event
handleProfilePageEvent currentState event = handleGenericPageEvent currentState event

handleGenericPageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleGenericPageEvent currentState be@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'q') []) -> resizeOrQuit currentState be
  _ -> continue currentState
handleGenericPageEvent currentState _ = continue currentState

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
  Just 2 -> continue (currentState & appPage .~ Profiles)
  _ -> continue currentState
  where
    selectedElement = listSelected (currentState ^. currentControlList)

handleProfileListSelect :: AppState -> EventM String (Next AppState)
handleProfileListSelect currentState = do
  case selectedElement of
    Just profileNum -> do
      let profileContainer =
            ProfileContainer
              (currentState ^. appProfiles)
              ((profileList !! profileNum) ^. profileId)
      liftIO $ saveProfileConfig profileContainer
      continue (setAppProfile (profileList !! profileNum) currentState & appPage .~ Main)
    Nothing -> continue currentState
  where
    profileList = getProfileListFromMap (currentState ^. appProfiles)
    selectedElement = listSelected (currentState ^. currentProfileList)

handleProfileListDelete :: AppState -> EventM String (Next AppState)
handleProfileListDelete currentState = do
  if M.size (currentState ^. appProfiles) <= 1
    then continue currentState
    else do
      case selectedElement of
        Just profileNum -> do
          let profile = profileList !! profileNum
              newAppProfiles = M.delete (profile ^. profileId) (currentState ^. appProfiles)
              newAppProfileList = getProfileListFromMap newAppProfiles
              newUIProfileList = makeProfileList newAppProfiles
              (newProfile, newState)
                | currentState ^. currentProfile == profile =
                    ( head newAppProfileList,
                      setAppProfile (head newAppProfileList) currentState
                    )
                | otherwise = (currentState ^. currentProfile, currentState)
              profileContainer =
                ProfileContainer
                  newAppProfiles
                  (newProfile ^. profileId)
          liftIO $ saveProfileConfig profileContainer
          continue
            ( newState
                & appProfiles .~ newAppProfiles
                & currentProfileList .~ newUIProfileList
            )
        Nothing -> continue currentState
  where
    profileList = getProfileListFromMap (currentState ^. appProfiles)
    selectedElement = listSelected (currentState ^. currentProfileList)

handleControlListEvents :: AppState -> V.Event -> EventM String (Next AppState)
handleControlListEvents currentState ve = do
  let controlList = currentState ^. currentControlList
  newControlList <- handleListEventVi handleListEvent ve controlList
  continue (currentState & currentControlList .~ newControlList)

handleProfileListEvents :: AppState -> V.Event -> EventM String (Next AppState)
handleProfileListEvents currentState ve = do
  let profileList = currentState ^. currentProfileList
  newProfileList <- handleListEventVi handleListEvent ve profileList
  continue (currentState & currentProfileList .~ newProfileList)

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
  _ <- liftIO $ sendNotification ("Time to Rest: " <> renderCurrentTimer (currentState ^. currentProfile . restTime))
  return $ currentState & currentPomodoroState .~ Rest & currentTimer .~ currentState ^. currentProfile . restTime

setPomodoroWork :: AppState -> EventM String AppState
setPomodoroWork currentState = do
  _ <- liftIO $ sendNotification ("Time to Work: " <> renderCurrentTimer (currentState ^. currentProfile . workTime))
  return $ currentState & currentPomodoroState .~ Work & currentTimer .~ currentState ^. currentProfile . workTime

setAppProfile :: Profile -> AppState -> AppState
setAppProfile newProfile state =
  state
    & currentProfile .~ newProfile
    & currentPomodoroState .~ Work
    & currentTimer .~ (newProfile ^. workTime)

buildInitState :: ProfileContainer -> AppState
buildInitState profileContainer =
  initialAppState
    & currentProfile .~ chosenProfile
    & currentPomodoroState .~ Work
    & currentTimer .~ (chosenProfile ^. workTime)
    & appProfiles .~ profiles profileContainer
    & currentProfileList .~ profileList
  where
    loadedProfiles = profiles profileContainer
    chosenProfile = fromMaybe defaultAppProfile (M.lookup (currentProfileId profileContainer) loadedProfiles)
    profileList = makeProfileList loadedProfiles

decreasePomodoroTimer :: AppState -> EventM String (Next AppState)
decreasePomodoroTimer currentState = continue $ currentState & currentTimer -~ 1

createApp :: App AppState PomodoroEvent String
createApp =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

initialAppState :: AppState
initialAppState =
  AppState
    { _currentPomodoroState = Work,
      _currentTimer = 1500,
      _currentControlList = makeControlList Work,
      _currentProfileList = makeProfileList (M.fromList [(defaultAppProfile ^. profileId, defaultAppProfile)]),
      _currentProfile = defaultAppProfile,
      _appProfiles = M.fromList [(defaultAppProfile ^. profileId, defaultAppProfile)],
      _appPage = Main
    }
