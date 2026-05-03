{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Settings
  ( Settings(..)
  , defaultSettings
  , jsonSettingsPort
  , loadSettings
  , saveSettings
  , viewFromText
  , viewToText
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import ZeitLingq.Domain.Types (View(..))
import ZeitLingq.Ports (SettingsPort(..))

data Settings = Settings
  { settingsCurrentView :: View
  , settingsBrowseSection :: Text
  , settingsDatePrefixEnabled :: Bool
  , settingsSectionCollections :: Map Text Text
  } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsCurrentView = BrowseView
    , settingsBrowseSection = "index"
    , settingsDatePrefixEnabled = True
    , settingsSectionCollections = Map.empty
    }

jsonSettingsPort :: FilePath -> SettingsPort IO
jsonSettingsPort path =
  SettingsPort
    { loadCurrentView = settingsCurrentView <$> loadSettings path
    , saveCurrentView = updateSettings path . setCurrentView
    , loadBrowseSection = settingsBrowseSection <$> loadSettings path
    , saveBrowseSection = updateSettings path . setBrowseSection
    , loadDatePrefixEnabled = settingsDatePrefixEnabled <$> loadSettings path
    , saveDatePrefixEnabled = updateSettings path . setDatePrefixEnabled
    , loadSectionCollections = settingsSectionCollections <$> loadSettings path
    , saveSectionCollections = updateSettings path . setSectionCollections
    }

loadSettings :: FilePath -> IO Settings
loadSettings path = do
  exists <- doesFileExist path
  if exists
    then do
      raw <- BS.readFile path
      pure (either (const defaultSettings) id (eitherDecodeStrict' raw))
    else pure defaultSettings

saveSettings :: FilePath -> Settings -> IO ()
saveSettings path settings = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode settings)

updateSettings :: FilePath -> (Settings -> Settings) -> IO ()
updateSettings path change = do
  settings <- loadSettings path
  saveSettings path (change settings)

setCurrentView :: View -> Settings -> Settings
setCurrentView value settings = settings {settingsCurrentView = value}

setBrowseSection :: Text -> Settings -> Settings
setBrowseSection value settings = settings {settingsBrowseSection = value}

setDatePrefixEnabled :: Bool -> Settings -> Settings
setDatePrefixEnabled value settings = settings {settingsDatePrefixEnabled = value}

setSectionCollections :: Map Text Text -> Settings -> Settings
setSectionCollections value settings = settings {settingsSectionCollections = value}

instance ToJSON Settings where
  toJSON settings =
    object
      [ "currentView" .= viewToText (settingsCurrentView settings)
      , "browseSection" .= settingsBrowseSection settings
      , "datePrefixEnabled" .= settingsDatePrefixEnabled settings
      , "sectionCollections" .= settingsSectionCollections settings
      ]

instance FromJSON Settings where
  parseJSON =
    withObject "Settings" $ \obj ->
      Settings
        <$> parseView obj
        <*> obj .:? "browseSection" .!= settingsBrowseSection defaultSettings
        <*> obj .:? "datePrefixEnabled" .!= settingsDatePrefixEnabled defaultSettings
        <*> obj .:? "sectionCollections" .!= settingsSectionCollections defaultSettings
    where
      parseView obj = do
        value <- obj .:? "currentView" .!= viewToText (settingsCurrentView defaultSettings)
        parseViewText value

parseViewText :: Text -> Parser View
parseViewText value =
  case viewFromText value of
    Just view -> pure view
    Nothing -> fail ("Unknown view: " <> T.unpack value)

viewToText :: View -> Text
viewToText BrowseView = "browse"
viewToText LibraryView = "library"
viewToText LingqView = "lingq"
viewToText ZeitLoginView = "zeit-login"
viewToText ArticleView = "article"

viewFromText :: Text -> Maybe View
viewFromText "browse" = Just BrowseView
viewFromText "library" = Just LibraryView
viewFromText "lingq" = Just LingqView
viewFromText "zeit-login" = Just ZeitLoginView
viewFromText "article" = Just ArticleView
viewFromText _ = Nothing
