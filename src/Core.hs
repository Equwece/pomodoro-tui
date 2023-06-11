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
import Data.UUID.V4 (nextRandom)
import Data.Vector (Vector, fromList)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)
import Types
  ( AppState (..),
    EditorModType (Decrease, Increase),
    Page (Main, ProfileCreator, ProfileEditor, Profiles),
    PomodoroEvent (..),
    PomodoroState (Pause, Rest, Work),
    Profile (..),
    ProfileContainer (ProfileContainer, currentProfileId, profiles),
    appPage,
    appProfiles,
    currentControlList,
    currentEditorList,
    currentPomodoroState,
    currentProfile,
    currentProfileList,
    currentTimer,
    defaultAppProfile,
    editingProfile,
    longRestCount,
    longRestTime,
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
makeProfileList state = list "ProfileList" (fromList (map renderProfileItem $ getProfileListFromMap state)) 1

makeEditorList :: Profile -> GenericList String Vector String
makeEditorList profile =
  list
    "EditorList"
    ( fromList
        [ "Work: " <> workTimeStr,
          "Rest: " <> restTimeStr,
          "Long Rest Count: " <> longRestCountStr,
          "Long Rest Time: " <> longRestTimeStr
        ]
    )
    1
  where
    workTimeStr = renderCurrentTimer (profile ^. workTime)
    restTimeStr = renderCurrentTimer (profile ^. restTime)
    longRestCountStr = maybe "Disabled" show (profile ^. longRestCount)
    longRestTimeStr = maybe "Disabled" renderCurrentTimer (profile ^. longRestTime)

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
  ProfileCreator -> renderEditorPage currentState
  ProfileEditor -> renderEditorPage currentState
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

renderEditorPage :: AppState -> [Widget String]
renderEditorPage currentState =
  [ center $
      vBox
        [ vLimit 15 $ renderList renderProfileListItem True (currentState ^. currentEditorList)
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
  ProfileCreator -> handleCreatorPageEvent currentState event
  ProfileEditor -> handleEditorPageEvent currentState event
  Profiles -> handleProfilePageEvent currentState event
  Main -> handleMainPageEvent currentState event

handleMainPageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleMainPageEvent currentState (AppEvent Second) = handleSecond currentState
handleMainPageEvent currentState event@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleControlListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleControlListEvents currentState ve
  (V.EvKey V.KUp []) -> handleControlListEvents currentState ve
  (V.EvKey V.KDown []) -> handleControlListEvents currentState ve
  (V.EvKey V.KEnter []) -> handleControlListSelect currentState
  (V.EvKey (V.KChar ' ') []) -> handleControlListSelect currentState
  _ -> handleGenericPageEvent currentState event
handleMainPageEvent currentState event = handleGenericPageEvent currentState event

handleProfilePageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleProfilePageEvent currentState (AppEvent Second) = handleSecond currentState
handleProfilePageEvent currentState event@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleProfileListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KUp []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KDown []) -> handleProfileListEvents currentState ve
  (V.EvKey V.KEsc []) -> continue (currentState & appPage .~ Main)
  (V.EvKey V.KEnter []) -> handleProfileListSelect currentState
  (V.EvKey (V.KChar ' ') []) -> handleProfileListSelect currentState
  (V.EvKey (V.KChar 'e') []) ->
    continue
      ( currentState
          & appPage .~ ProfileEditor
          & currentEditorList .~ makeEditorList newEditingProfile
          & editingProfile .~ newEditingProfile
      )
    where
      selectedElementNum = fromMaybe 0 (listSelected (currentState ^. currentProfileList))
      newEditingProfile = getProfileListFromMap (currentState ^. appProfiles) !! selectedElementNum
  (V.EvKey (V.KChar 'n') []) ->
    continue
      ( currentState
          & appPage .~ ProfileCreator
          & currentEditorList .~ makeEditorList defaultAppProfile
          & editingProfile .~ defaultAppProfile
      )
  (V.EvKey (V.KChar 'd') []) -> handleProfileListDelete currentState
  _ -> handleGenericPageEvent currentState event
handleProfilePageEvent currentState event = handleGenericPageEvent currentState event

handleEditorPageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleEditorPageEvent currentState (AppEvent Second) = handleSecond currentState
handleEditorPageEvent currentState event@(VtyEvent ve) = case ve of
  (V.EvKey (V.KChar 'j') []) -> handleEditorListEvents currentState ve
  (V.EvKey (V.KChar 'k') []) -> handleEditorListEvents currentState ve
  (V.EvKey (V.KChar 'l') []) -> handleEditorMod currentState Increase
  (V.EvKey V.KRight []) -> handleEditorMod currentState Increase
  (V.EvKey (V.KChar 'h') []) -> handleEditorMod currentState Decrease
  (V.EvKey (V.KChar 's') []) -> do
    _ <- saveCurrentAppState currentState
    continue
      ( currentState
          & appPage .~ Profiles
          & currentProfileList .~ makeProfileList (currentState ^. appProfiles)
      )
  (V.EvKey V.KLeft []) -> handleEditorMod currentState Decrease
  (V.EvKey V.KUp []) -> handleEditorListEvents currentState ve
  (V.EvKey V.KDown []) -> handleEditorListEvents currentState ve
  (V.EvKey V.KEsc []) -> do
    continue
      ( currentState
          & appPage .~ Profiles
          & currentProfileList .~ makeProfileList (currentState ^. appProfiles)
      )
  _ -> handleGenericPageEvent currentState event
handleEditorPageEvent currentState event = handleGenericPageEvent currentState event

handleCreatorPageEvent :: AppState -> BrickEvent String PomodoroEvent -> EventM String (Next AppState)
handleCreatorPageEvent currentState (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  newId <- liftIO nextRandom
  let newProfile = (currentState ^. editingProfile) {_profileId = newId}
      newAppProfiles = M.insert newId newProfile (currentState ^. appProfiles)
      newAppState = currentState & appProfiles .~ newAppProfiles
  _ <- saveCurrentAppState newAppState
  continue
    ( newAppState
        & appPage .~ Profiles
        & currentProfileList .~ makeProfileList (newAppState ^. appProfiles)
    )
handleCreatorPageEvent currentState event = handleEditorPageEvent currentState event

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
  Just 2 ->
    continue
      ( currentState
          & appPage .~ Profiles
          & currentProfileList .~ makeProfileList (currentState ^. appProfiles)
      )
  _ -> continue currentState
  where
    selectedElement = listSelected (currentState ^. currentControlList)

handleProfileListSelect :: AppState -> EventM String (Next AppState)
handleProfileListSelect currentState = case selectedElement of
  Just profileNum -> do
    let profileContainer =
          ProfileContainer
            (currentState ^. appProfiles)
            (profileList !! profileNum ^. profileId)
    liftIO $ saveProfileConfig profileContainer
    continue (setAppProfile (profileList !! profileNum) currentState & appPage .~ Main)
  Nothing -> continue currentState
  where
    profileList = getProfileListFromMap (currentState ^. appProfiles)
    selectedElement = listSelected (currentState ^. currentProfileList)

handleProfileListDelete :: AppState -> EventM String (Next AppState)
handleProfileListDelete currentState =
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

handleEditorMod :: AppState -> EditorModType -> EventM String (Next AppState)
handleEditorMod currentState modType = continue result
  where
    modifier 0 = case modType of
      Increase -> 1
      Decrease -> 0
    modifier a = case modType of
      Increase -> a + 1
      Decrease -> a - 1
    longRestModifier (Just x) = case modType of
      Increase -> Just (x + 1)
      Decrease -> if x <= 1 then Nothing else Just (x - 1)
    longRestModifier Nothing = case modType of
      Increase -> Just 1
      Decrease -> Nothing

    longRestUpdate updateResult = checkResult
      where
        isEnabled (Just _) = True
        isEnabled Nothing = False
        checkResult
          | isEnabled (updateResult ^. longRestTime)
              && isEnabled (updateResult ^. longRestCount) =
              updateResult
          | otherwise = case modType of
              Decrease -> updateResult & longRestCount .~ Nothing & longRestTime .~ Nothing
              Increase -> updateResult & longRestCount .~ Just 1 & longRestTime .~ Just 0

    selectedElement = listSelected (currentState ^. currentEditorList)
    profile = currentState ^. editingProfile
    updatedProfile = case selectedElement of
      Just 0 -> profile & workTime .~ modifier (profile ^. workTime)
      Just 1 -> profile & restTime .~ modifier (profile ^. restTime)
      Just 2 -> longRestUpdate $ profile & longRestCount .~ longRestModifier (profile ^. longRestCount)
      Just 3 -> longRestUpdate $ profile & longRestTime .~ longRestModifier (profile ^. longRestTime)
      _ -> profile
    newUIlist = (makeEditorList updatedProfile) {listSelected = selectedElement}
    newProfileMap = M.update (const (Just updatedProfile)) (updatedProfile ^. profileId) (currentState ^. appProfiles)
    result =
      currentState
        & editingProfile .~ updatedProfile
        & currentEditorList .~ newUIlist
        & appProfiles .~ newProfileMap

saveCurrentAppState :: AppState -> EventM String (Next AppState)
saveCurrentAppState appState = do
  let profileContainer = ProfileContainer (appState ^. appProfiles) (appState ^. currentProfile . profileId)
  liftIO $ saveProfileConfig profileContainer
  continue appState

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

handleEditorListEvents :: AppState -> V.Event -> EventM String (Next AppState)
handleEditorListEvents currentState ve = do
  let editorList = currentState ^. currentEditorList
  newEditorList <- handleListEventVi handleListEvent ve editorList
  continue (currentState & currentEditorList .~ newEditorList)

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
    & currentTimer .~ newProfile ^. workTime

buildInitState :: ProfileContainer -> AppState
buildInitState profileContainer =
  initialAppState
    & currentProfile .~ chosenProfile
    & currentPomodoroState .~ Work
    & currentTimer .~ chosenProfile ^. workTime
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
      _currentEditorList = makeEditorList defaultAppProfile,
      _editingProfile = defaultAppProfile,
      _appPage = Main
    }
