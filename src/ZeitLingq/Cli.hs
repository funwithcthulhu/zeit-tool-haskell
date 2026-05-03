{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Cli
  ( CliCommand(..)
  , defaultDbPath
  , defaultSettingsPath
  , parseArgs
  , usageText
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import ZeitLingq.Domain.Types (View(..), WordFilter(..))

data CliCommand
  = ShowDemo
  | ListSections
  | BrowseZeit Text Int FilePath
  | FetchArticle Text FilePath
  | BatchFetch FilePath FilePath WordFilter
  | ShowLibrary FilePath
  | ShowStats FilePath
  | DeleteArticle Int FilePath
  | DeleteOlderThan Int Bool Bool FilePath
  | DeleteIgnored FilePath
  | IgnoreArticle Int FilePath
  | UnignoreArticle Int FilePath
  | SyncKnownWords FilePath
  | ImportKnownWords FilePath FilePath
  | KnownWordsInfo FilePath
  | ComputeKnownPct FilePath
  | UploadLingq Int FilePath FilePath
  | DownloadAudio Int FilePath FilePath
  | IgnoreUrl Text FilePath
  | UnignoreUrl Text FilePath
  | ListIgnored FilePath
  | ShowSettings FilePath
  | SetSettingsView View FilePath
  | SetSettingsBrowseSection Text FilePath
  | SetSettingsDatePrefix Bool FilePath
  | SetSettingsSectionCollection Text Text FilePath
  | ClearSettingsSectionCollection Text FilePath
  deriving (Eq, Show)

defaultDbPath :: FilePath
defaultDbPath = "zeit-tool.db"

defaultSettingsPath :: FilePath
defaultSettingsPath = "settings.json"

parseArgs :: [String] -> Either String CliCommand
parseArgs [] = Right ShowDemo
parseArgs ["sections"] = Right ListSections
parseArgs ["browse", sectionId] = Right (BrowseZeit (T.pack sectionId) 1 defaultDbPath)
parseArgs ["browse", sectionId, pageValue] =
  BrowseZeit (T.pack sectionId)
    <$> parsePositiveInt "page" pageValue
    <*> pure defaultDbPath
parseArgs ["browse", sectionId, pageValue, dbPath] =
  BrowseZeit (T.pack sectionId)
    <$> parsePositiveInt "page" pageValue
    <*> pure dbPath
parseArgs ["fetch", url] = Right (FetchArticle (T.pack url) defaultDbPath)
parseArgs ["fetch", url, dbPath] = Right (FetchArticle (T.pack url) dbPath)
parseArgs ["batch-fetch", sourcePath] = Right (BatchFetch sourcePath defaultDbPath (WordFilter Nothing Nothing))
parseArgs ["batch-fetch", sourcePath, dbPath] = Right (BatchFetch sourcePath dbPath (WordFilter Nothing Nothing))
parseArgs ["batch-fetch", sourcePath, dbPath, minValue, maxValue] =
  BatchFetch sourcePath dbPath
    <$> (WordFilter
          <$> fmap Just (parsePositiveInt "min-words" minValue)
          <*> fmap Just (parsePositiveInt "max-words" maxValue))
parseArgs ["library"] = Right (ShowLibrary defaultDbPath)
parseArgs ["library", dbPath] = Right (ShowLibrary dbPath)
parseArgs ["stats"] = Right (ShowStats defaultDbPath)
parseArgs ["stats", dbPath] = Right (ShowStats dbPath)
parseArgs ["delete-article", articleId] =
  DeleteArticle <$> parsePositiveInt "article-id" articleId <*> pure defaultDbPath
parseArgs ["delete-article", articleId, dbPath] =
  DeleteArticle <$> parsePositiveInt "article-id" articleId <*> pure dbPath
parseArgs ["delete-older-than", days] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure False <*> pure False <*> pure defaultDbPath
parseArgs ["delete-older-than", days, dbPath] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure False <*> pure False <*> pure dbPath
parseArgs ["delete-older-than-uploaded", days] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure True <*> pure False <*> pure defaultDbPath
parseArgs ["delete-older-than-uploaded", days, dbPath] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure True <*> pure False <*> pure dbPath
parseArgs ["delete-older-than-unuploaded", days] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure False <*> pure True <*> pure defaultDbPath
parseArgs ["delete-older-than-unuploaded", days, dbPath] =
  DeleteOlderThan <$> parsePositiveInt "days" days <*> pure False <*> pure True <*> pure dbPath
parseArgs ["delete-ignored"] = Right (DeleteIgnored defaultDbPath)
parseArgs ["delete-ignored", dbPath] = Right (DeleteIgnored dbPath)
parseArgs ["ignore-article", articleId] =
  IgnoreArticle <$> parsePositiveInt "article-id" articleId <*> pure defaultDbPath
parseArgs ["ignore-article", articleId, dbPath] =
  IgnoreArticle <$> parsePositiveInt "article-id" articleId <*> pure dbPath
parseArgs ["unignore-article", articleId] =
  UnignoreArticle <$> parsePositiveInt "article-id" articleId <*> pure defaultDbPath
parseArgs ["unignore-article", articleId, dbPath] =
  UnignoreArticle <$> parsePositiveInt "article-id" articleId <*> pure dbPath
parseArgs ["known-sync"] = Right (SyncKnownWords defaultDbPath)
parseArgs ["known-sync", dbPath] = Right (SyncKnownWords dbPath)
parseArgs ["known-import", sourcePath] = Right (ImportKnownWords sourcePath defaultDbPath)
parseArgs ["known-import", sourcePath, dbPath] = Right (ImportKnownWords sourcePath dbPath)
parseArgs ["known-info"] = Right (KnownWordsInfo defaultDbPath)
parseArgs ["known-info", dbPath] = Right (KnownWordsInfo dbPath)
parseArgs ["known-compute"] = Right (ComputeKnownPct defaultDbPath)
parseArgs ["known-compute", dbPath] = Right (ComputeKnownPct dbPath)
parseArgs ["lingq-upload", articleId] =
  UploadLingq
    <$> parsePositiveInt "article-id" articleId
    <*> pure defaultDbPath
    <*> pure defaultSettingsPath
parseArgs ["lingq-upload", articleId, dbPath] =
  UploadLingq
    <$> parsePositiveInt "article-id" articleId
    <*> pure dbPath
    <*> pure defaultSettingsPath
parseArgs ["lingq-upload", articleId, dbPath, settingsPath] =
  UploadLingq
    <$> parsePositiveInt "article-id" articleId
    <*> pure dbPath
    <*> pure settingsPath
parseArgs ["audio-download", articleId] =
  DownloadAudio <$> parsePositiveInt "article-id" articleId <*> pure "audio" <*> pure defaultDbPath
parseArgs ["audio-download", articleId, audioDir] =
  DownloadAudio <$> parsePositiveInt "article-id" articleId <*> pure audioDir <*> pure defaultDbPath
parseArgs ["audio-download", articleId, audioDir, dbPath] =
  DownloadAudio <$> parsePositiveInt "article-id" articleId <*> pure audioDir <*> pure dbPath
parseArgs ["ignore-url", url] = Right (IgnoreUrl (T.pack url) defaultDbPath)
parseArgs ["ignore-url", url, dbPath] = Right (IgnoreUrl (T.pack url) dbPath)
parseArgs ["unignore-url", url] = Right (UnignoreUrl (T.pack url) defaultDbPath)
parseArgs ["unignore-url", url, dbPath] = Right (UnignoreUrl (T.pack url) dbPath)
parseArgs ["ignored"] = Right (ListIgnored defaultDbPath)
parseArgs ["ignored", dbPath] = Right (ListIgnored dbPath)
parseArgs ["settings"] = Right (ShowSettings defaultSettingsPath)
parseArgs ["settings", "set-view", viewValue] =
  SetSettingsView <$> parseView "view" viewValue <*> pure defaultSettingsPath
parseArgs ["settings", "set-view", viewValue, settingsPath] =
  SetSettingsView <$> parseView "view" viewValue <*> pure settingsPath
parseArgs ["settings", "set-browse-section", sectionId] =
  Right (SetSettingsBrowseSection (T.pack sectionId) defaultSettingsPath)
parseArgs ["settings", "set-browse-section", sectionId, settingsPath] =
  Right (SetSettingsBrowseSection (T.pack sectionId) settingsPath)
parseArgs ["settings", "set-date-prefix", enabledValue] =
  SetSettingsDatePrefix <$> parseToggle "date-prefix" enabledValue <*> pure defaultSettingsPath
parseArgs ["settings", "set-date-prefix", enabledValue, settingsPath] =
  SetSettingsDatePrefix <$> parseToggle "date-prefix" enabledValue <*> pure settingsPath
parseArgs ["settings", "set-collection", sectionName, collectionId] =
  Right (SetSettingsSectionCollection (T.pack sectionName) (T.pack collectionId) defaultSettingsPath)
parseArgs ["settings", "set-collection", sectionName, collectionId, settingsPath] =
  Right (SetSettingsSectionCollection (T.pack sectionName) (T.pack collectionId) settingsPath)
parseArgs ["settings", "clear-collection", sectionName] =
  Right (ClearSettingsSectionCollection (T.pack sectionName) defaultSettingsPath)
parseArgs ["settings", "clear-collection", sectionName, settingsPath] =
  Right (ClearSettingsSectionCollection (T.pack sectionName) settingsPath)
parseArgs ["settings", settingsPath] = Right (ShowSettings settingsPath)
parseArgs _ = Left usageText

usageText :: String
usageText =
  unlines
    [ "Usage:"
    , "  zeit-lingq-tool"
    , "  zeit-lingq-tool sections"
    , "  zeit-lingq-tool browse <section-id> [page] [db-path]"
    , "  zeit-lingq-tool fetch <article-url> [db-path]"
    , "  zeit-lingq-tool batch-fetch <url-list.txt> [db-path]"
    , "  zeit-lingq-tool batch-fetch <url-list.txt> <db-path> <min-words> <max-words>"
    , "  zeit-lingq-tool library [db-path]"
    , "  zeit-lingq-tool stats [db-path]"
    , "  zeit-lingq-tool delete-article <article-id> [db-path]"
    , "  zeit-lingq-tool delete-older-than <days> [db-path]"
    , "  zeit-lingq-tool delete-older-than-uploaded <days> [db-path]"
    , "  zeit-lingq-tool delete-older-than-unuploaded <days> [db-path]"
    , "  zeit-lingq-tool delete-ignored [db-path]"
    , "  zeit-lingq-tool ignore-article <article-id> [db-path]"
    , "  zeit-lingq-tool unignore-article <article-id> [db-path]"
    , "  zeit-lingq-tool known-sync [db-path]"
    , "  zeit-lingq-tool known-import <word-list.txt> [db-path]"
    , "  zeit-lingq-tool known-info [db-path]"
    , "  zeit-lingq-tool known-compute [db-path]"
    , "  zeit-lingq-tool lingq-upload <article-id> [db-path] [settings-path]"
    , "  zeit-lingq-tool audio-download <article-id> [audio-dir] [db-path]"
    , "  zeit-lingq-tool ignore-url <url> [db-path]"
    , "  zeit-lingq-tool unignore-url <url> [db-path]"
    , "  zeit-lingq-tool ignored [db-path]"
    , "  zeit-lingq-tool settings [settings-path]"
    , "  zeit-lingq-tool settings set-view <browse|library|lingq|zeit-login|article> [settings-path]"
    , "  zeit-lingq-tool settings set-browse-section <section-id> [settings-path]"
    , "  zeit-lingq-tool settings set-date-prefix <on|off> [settings-path]"
    , "  zeit-lingq-tool settings set-collection <section-name> <collection-id> [settings-path]"
    , "  zeit-lingq-tool settings clear-collection <section-name> [settings-path]"
    , ""
    , "Set ZEIT_COOKIE to pass an authenticated zeit.de cookie header for paid articles."
    , "Set ZEIT_USER_AGENT too when scripting with cookies imported from a browser."
    , "Set LINGQ_API_KEY, and optionally LINGQ_COLLECTION_ID, before uploading to LingQ."
    ]

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label raw =
  case reads raw of
    [(value, "")] | value > 0 -> Right value
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)

parseToggle :: String -> String -> Either String Bool
parseToggle label raw =
  case T.toLower (T.pack raw) of
    "on" -> Right True
    "true" -> Right True
    "yes" -> Right True
    "1" -> Right True
    "enabled" -> Right True
    "off" -> Right False
    "false" -> Right False
    "no" -> Right False
    "0" -> Right False
    "disabled" -> Right False
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)

parseView :: String -> String -> Either String View
parseView label raw =
  case T.toLower (T.pack raw) of
    "browse" -> Right BrowseView
    "library" -> Right LibraryView
    "lingq" -> Right LingqView
    "zeit-login" -> Right ZeitLoginView
    "article" -> Right ArticleView
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)
