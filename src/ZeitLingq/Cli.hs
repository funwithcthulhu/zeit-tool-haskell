{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Cli
  ( CliCommand(..)
  , defaultDbPath
  , parseArgs
  , usageText
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import ZeitLingq.Domain.Types (WordFilter(..))

data CliCommand
  = ShowDemo
  | ListSections
  | BrowseZeit Text Int
  | FetchArticle Text FilePath
  | BatchFetch FilePath FilePath WordFilter
  | ShowLibrary FilePath
  | ImportKnownWords FilePath FilePath
  | KnownWordsInfo FilePath
  | ComputeKnownPct FilePath
  | UploadLingq Int FilePath
  | DownloadAudio Int FilePath FilePath
  | IgnoreUrl Text FilePath
  | UnignoreUrl Text FilePath
  | ListIgnored FilePath
  deriving (Eq, Show)

defaultDbPath :: FilePath
defaultDbPath = "zeit-tool.db"

parseArgs :: [String] -> Either String CliCommand
parseArgs [] = Right ShowDemo
parseArgs ["sections"] = Right ListSections
parseArgs ["browse", sectionId] = Right (BrowseZeit (T.pack sectionId) 1)
parseArgs ["browse", sectionId, pageValue] =
  BrowseZeit (T.pack sectionId) <$> parsePositiveInt "page" pageValue
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
parseArgs ["known-import", sourcePath] = Right (ImportKnownWords sourcePath defaultDbPath)
parseArgs ["known-import", sourcePath, dbPath] = Right (ImportKnownWords sourcePath dbPath)
parseArgs ["known-info"] = Right (KnownWordsInfo defaultDbPath)
parseArgs ["known-info", dbPath] = Right (KnownWordsInfo dbPath)
parseArgs ["known-compute"] = Right (ComputeKnownPct defaultDbPath)
parseArgs ["known-compute", dbPath] = Right (ComputeKnownPct dbPath)
parseArgs ["lingq-upload", articleId] =
  UploadLingq <$> parsePositiveInt "article-id" articleId <*> pure defaultDbPath
parseArgs ["lingq-upload", articleId, dbPath] =
  UploadLingq <$> parsePositiveInt "article-id" articleId <*> pure dbPath
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
parseArgs _ = Left usageText

usageText :: String
usageText =
  unlines
    [ "Usage:"
    , "  zeit-lingq-tool"
    , "  zeit-lingq-tool sections"
    , "  zeit-lingq-tool browse <section-id> [page]"
    , "  zeit-lingq-tool fetch <article-url> [db-path]"
    , "  zeit-lingq-tool batch-fetch <url-list.txt> [db-path]"
    , "  zeit-lingq-tool batch-fetch <url-list.txt> <db-path> <min-words> <max-words>"
    , "  zeit-lingq-tool library [db-path]"
    , "  zeit-lingq-tool known-import <word-list.txt> [db-path]"
    , "  zeit-lingq-tool known-info [db-path]"
    , "  zeit-lingq-tool known-compute [db-path]"
    , "  zeit-lingq-tool lingq-upload <article-id> [db-path]"
    , "  zeit-lingq-tool audio-download <article-id> [audio-dir] [db-path]"
    , "  zeit-lingq-tool ignore-url <url> [db-path]"
    , "  zeit-lingq-tool unignore-url <url> [db-path]"
    , "  zeit-lingq-tool ignored [db-path]"
    , ""
    , "Set ZEIT_COOKIE to pass an authenticated zeit.de cookie header for paid articles."
    , "Set LINGQ_API_KEY, and optionally LINGQ_COLLECTION_ID, before uploading to LingQ."
    ]

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label raw =
  case reads raw of
    [(value, "")] | value > 0 -> Right value
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)
