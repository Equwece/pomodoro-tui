{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Brick.Widgets.List (GenericList)
import Control.Lens
import Data.Map
import Data.UUID (UUID, nil)
import Data.Vector (Vector)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

data AppState = AppState
  { _currentPomodoroState :: PomodoroState,
    _currentTimer :: Int,
    _currentControlList :: GenericList String Vector String,
    _currentProfileList :: GenericList String Vector String,
    _currentProfile :: Profile,
    _appProfiles :: Map UUID Profile,
    _appPage :: Page
  }
  deriving (Show)

data Page = Main | Profiles | ProfileEditor deriving (Show)

data Profile = Profile
  { _workTime :: Int,
    _restTime :: Int,
    _longRestTime :: Maybe Int,
    _profileId :: UUID,
    _longRestCount :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Profile

instance FromJSON Profile

data ProfileContainer = ProfileContainer {profiles :: Map UUID Profile, currentProfileId :: UUID} deriving (Show, Generic)

instance ToJSON ProfileContainer

instance FromJSON ProfileContainer

data PomodoroState = Work | Rest | LongRest | Pause PomodoroState deriving (Eq, Show)

data PomodoroEvent = Second

makeLenses ''AppState
makeLenses ''Profile

defaultAppProfile :: Profile
defaultAppProfile =
  Profile
    { _workTime = 1500,
      _restTime = 300,
      _longRestTime = Just 900,
      _longRestCount = Just 4,
      _profileId = nil
    }

defaultProfileContainer :: ProfileContainer
defaultProfileContainer =
  ProfileContainer
    { profiles = fromList [(defaultAppProfile ^. profileId, defaultAppProfile)],
      currentProfileId = defaultAppProfile ^. profileId
    }
