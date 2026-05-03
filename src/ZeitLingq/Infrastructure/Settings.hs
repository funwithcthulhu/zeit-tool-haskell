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
import ZeitLingq.Domain.Types (View(..), WordFilter(..))
import ZeitLingq.Ports (SettingsPort(..))

data Settings = Settings
  { settingsCurrentView :: View
  , settingsZeitCookie :: Text
  , settingsLingqApiKey :: Text
  , settingsLingqLanguage :: Text
  , settingsBrowseSection :: Text
  , settingsBrowseFilter :: WordFilter
  , settingsBrowseOnlyNew :: Bool
  , settingsLingqFilter :: WordFilter
  , settingsLingqOnlyNotUploaded :: Bool
  , settingsDatePrefixEnabled :: Bool
  , settingsLingqFallbackCollection :: Maybe Text
  , settingsSectionCollections :: Map Text Text
  } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsCurrentView = BrowseView
    , settingsZeitCookie = ""
    , settingsLingqApiKey = ""
    , settingsLingqLanguage = "de"
    , settingsBrowseSection = "index"
    , settingsBrowseFilter = WordFilter Nothing Nothing
    , settingsBrowseOnlyNew = True
    , settingsLingqFilter = WordFilter Nothing Nothing
    , settingsLingqOnlyNotUploaded = True
    , settingsDatePrefixEnabled = True
    , settingsLingqFallbackCollection = Nothing
    , settingsSectionCollections = Map.empty
    }

jsonSettingsPort :: FilePath -> SettingsPort IO
jsonSettingsPort path =
  SettingsPort
    { loadCurrentView = settingsCurrentView <$> loadSettings path
    , saveCurrentView = updateSettings path . setCurrentView
    , loadZeitCookie = settingsZeitCookie <$> loadSettings path
    , saveZeitCookie = updateSettings path . setZeitCookie
    , loadLingqApiKey = settingsLingqApiKey <$> loadSettings path
    , saveLingqApiKey = updateSettings path . setLingqApiKey
    , loadLingqLanguage = settingsLingqLanguage <$> loadSettings path
    , saveLingqLanguage = updateSettings path . setLingqLanguage
    , loadBrowseSection = settingsBrowseSection <$> loadSettings path
    , saveBrowseSection = updateSettings path . setBrowseSection
    , loadBrowseFilter = settingsBrowseFilter <$> loadSettings path
    , saveBrowseFilter = updateSettings path . setBrowseFilter
    , loadBrowseOnlyNew = settingsBrowseOnlyNew <$> loadSettings path
    , saveBrowseOnlyNew = updateSettings path . setBrowseOnlyNew
    , loadLingqFilter = settingsLingqFilter <$> loadSettings path
    , saveLingqFilter = updateSettings path . setLingqFilter
    , loadLingqOnlyNotUploaded = settingsLingqOnlyNotUploaded <$> loadSettings path
    , saveLingqOnlyNotUploaded = updateSettings path . setLingqOnlyNotUploaded
    , loadDatePrefixEnabled = settingsDatePrefixEnabled <$> loadSettings path
    , saveDatePrefixEnabled = updateSettings path . setDatePrefixEnabled
    , loadLingqFallbackCollection = settingsLingqFallbackCollection <$> loadSettings path
    , saveLingqFallbackCollection = updateSettings path . setLingqFallbackCollection
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

setZeitCookie :: Text -> Settings -> Settings
setZeitCookie value settings = settings {settingsZeitCookie = T.strip value}

setLingqApiKey :: Text -> Settings -> Settings
setLingqApiKey value settings = settings {settingsLingqApiKey = T.strip value}

setLingqLanguage :: Text -> Settings -> Settings
setLingqLanguage value settings = settings {settingsLingqLanguage = normalizeLingqLanguage value}

setBrowseSection :: Text -> Settings -> Settings
setBrowseSection value settings = settings {settingsBrowseSection = value}

setBrowseFilter :: WordFilter -> Settings -> Settings
setBrowseFilter value settings = settings {settingsBrowseFilter = value}

setBrowseOnlyNew :: Bool -> Settings -> Settings
setBrowseOnlyNew value settings = settings {settingsBrowseOnlyNew = value}

setLingqFilter :: WordFilter -> Settings -> Settings
setLingqFilter value settings = settings {settingsLingqFilter = value}

setLingqOnlyNotUploaded :: Bool -> Settings -> Settings
setLingqOnlyNotUploaded value settings = settings {settingsLingqOnlyNotUploaded = value}

setDatePrefixEnabled :: Bool -> Settings -> Settings
setDatePrefixEnabled value settings = settings {settingsDatePrefixEnabled = value}

setLingqFallbackCollection :: Maybe Text -> Settings -> Settings
setLingqFallbackCollection value settings = settings {settingsLingqFallbackCollection = value}

setSectionCollections :: Map Text Text -> Settings -> Settings
setSectionCollections value settings = settings {settingsSectionCollections = value}

instance ToJSON Settings where
  toJSON settings =
    object
      [ "currentView" .= viewToText (settingsCurrentView settings)
      , "zeitCookie" .= settingsZeitCookie settings
      , "lingqApiKey" .= settingsLingqApiKey settings
      , "lingqLanguage" .= settingsLingqLanguage settings
      , "browseSection" .= settingsBrowseSection settings
      , "browseFilter" .= wordFilterToJson (settingsBrowseFilter settings)
      , "browseOnlyNew" .= settingsBrowseOnlyNew settings
      , "lingqFilter" .= wordFilterToJson (settingsLingqFilter settings)
      , "lingqOnlyNotUploaded" .= settingsLingqOnlyNotUploaded settings
      , "datePrefixEnabled" .= settingsDatePrefixEnabled settings
      , "lingqFallbackCollection" .= settingsLingqFallbackCollection settings
      , "sectionCollections" .= settingsSectionCollections settings
      ]

instance FromJSON Settings where
  parseJSON =
    withObject "Settings" $ \obj ->
      Settings
        <$> parseView obj
        <*> obj .:? "zeitCookie" .!= settingsZeitCookie defaultSettings
        <*> obj .:? "lingqApiKey" .!= settingsLingqApiKey defaultSettings
        <*> (normalizeLingqLanguage <$> (obj .:? "lingqLanguage" .!= settingsLingqLanguage defaultSettings))
        <*> obj .:? "browseSection" .!= settingsBrowseSection defaultSettings
        <*> (obj .:? "browseFilter" >>= maybe (pure (settingsBrowseFilter defaultSettings)) parseWordFilter)
        <*> obj .:? "browseOnlyNew" .!= settingsBrowseOnlyNew defaultSettings
        <*> (obj .:? "lingqFilter" >>= maybe (pure (settingsLingqFilter defaultSettings)) parseWordFilter)
        <*> obj .:? "lingqOnlyNotUploaded" .!= settingsLingqOnlyNotUploaded defaultSettings
        <*> obj .:? "datePrefixEnabled" .!= settingsDatePrefixEnabled defaultSettings
        <*> obj .:? "lingqFallbackCollection" .!= settingsLingqFallbackCollection defaultSettings
        <*> obj .:? "sectionCollections" .!= settingsSectionCollections defaultSettings
    where
      parseView obj = do
        value <- obj .:? "currentView" .!= viewToText (settingsCurrentView defaultSettings)
        parseViewText value

wordFilterToJson :: WordFilter -> Value
wordFilterToJson filterValue =
  object
    [ "minWords" .= minWords filterValue
    , "maxWords" .= maxWords filterValue
    ]

parseWordFilter :: Value -> Parser WordFilter
parseWordFilter =
  withObject "WordFilter" $ \obj ->
    WordFilter
      <$> obj .:? "minWords"
      <*> obj .:? "maxWords"

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

normalizeLingqLanguage :: Text -> Text
normalizeLingqLanguage value
  | T.null stripped = settingsLingqLanguage defaultSettings
  | otherwise = stripped
  where
    stripped = T.toLower (T.strip value)
