{-# LANGUAGE OverloadedStrings #-}

module Utils (sendNotification, saveProfileConfig, loadProfileConfig) where

import DBus
  ( IsVariant (fromVariant, toVariant),
    MethodCall (methodCallBody, methodCallDestination),
    MethodReturn (methodReturnBody),
    methodCall,
  )
import DBus.Client (call_, connectSession)
import DBus.Internal.Types (Variant)
import Data.Int (Int32)
import Data.Map (Map, empty)
import Data.UUID (UUID)
import Data.Word (Word32)
import Data.Yaml (ParseException, decodeFileEither, encodeFile)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import Types (Profile, ProfileContainer (ProfileContainer, profiles))

makeConfigDirectoryIfMissing :: IO FilePath
makeConfigDirectoryIfMissing = do
  configDirPath <- getXdgDirectory XdgConfig "pomodoro-tui"
  createDirectoryIfMissing True configDirPath
  return configDirPath

saveProfileConfig :: ProfileContainer -> IO ()
saveProfileConfig profileContainer = do
  configPath <- (<> "/profiles.yaml") <$> makeConfigDirectoryIfMissing
  encodeFile configPath profileContainer

loadProfileConfig :: IO (Either ParseException ProfileContainer)
loadProfileConfig = do
  configPath <- (<> "/profiles.yaml") <$> makeConfigDirectoryIfMissing
  decodeFileEither configPath

sendNotification :: String -> IO (Maybe Word32)
sendNotification messageStr = do
  let callBody =
        [ toVariant ("Pomodoro tui" :: String),
          toVariant (0 :: Word32),
          toVariant ("" :: String),
          toVariant (messageStr :: String),
          toVariant ([] :: String),
          toVariant ([] :: [String]),
          toVariant (empty :: Map String Variant),
          toVariant (-1 :: Int32)
        ]

  client <- connectSession
  reply <-
    call_
      client
      (methodCall "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "Notify")
        { methodCallDestination = Just "org.freedesktop.Notifications",
          methodCallBody = callBody
        }

  case fromVariant (head (methodReturnBody reply)) :: Maybe Word32 of
    Just x -> return $ Just x
    Nothing -> return Nothing
