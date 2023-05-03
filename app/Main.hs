module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Core (PomodoroEvent (Second), createApp, initalAppState)
import qualified Graphics.Vty as V

main :: IO ()
main = do
  let app = createApp
      initialState = initalAppState
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan Second
    threadDelay 1000000
  void $ customMain initialVty buildVty (Just eventChan) app initialState
