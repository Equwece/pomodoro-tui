{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (customMain)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((&), (.~))
import Control.Monad (forever, void)
import Core (createApp, initialAppState, setAppProfile)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Types
  ( PomodoroEvent (Second),
    ProfileContainer (currentProfileId, profiles),
    appProfiles,
    defaultAppProfile,
    defaultProfileContainer,
  )
import Utils (loadProfileConfig)

main :: IO ()
main = do
  profileContainer <- fromRight defaultProfileContainer <$> loadProfileConfig
  let app = createApp
      buildVty = V.mkVty V.defaultConfig
      chosenProfile = fromMaybe defaultAppProfile (M.lookup (currentProfileId profileContainer) (profiles profileContainer))
      state =
        setAppProfile chosenProfile $
          initialAppState
            & appProfiles .~ profiles profileContainer
  initialVty <- buildVty
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan Second
    threadDelay 1000000 -- Event every second
  void $ customMain initialVty buildVty (Just eventChan) app state
