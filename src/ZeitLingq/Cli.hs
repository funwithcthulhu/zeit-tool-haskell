{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Cli
  ( CliCommand(..)
  , defaultDbPath
  , parseArgs
  , usageText
  ) where

import Data.Text (Text)
import Data.Text qualified as T

data CliCommand
  = ShowDemo
  | ListSections
  | BrowseZeit Text Int
  | FetchArticle Text FilePath
  | ShowLibrary FilePath
  | ImportKnownWords FilePath FilePath
  | KnownWordsInfo FilePath
  | ComputeKnownPct FilePath
  | UploadLingq Int FilePath
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
parseArgs _ = Left usageText

usageText :: String
usageText =
  unlines
    [ "Usage:"
    , "  zeit-lingq-tool"
    , "  zeit-lingq-tool sections"
    , "  zeit-lingq-tool browse <section-id> [page]"
    , "  zeit-lingq-tool fetch <article-url> [db-path]"
    , "  zeit-lingq-tool library [db-path]"
    , "  zeit-lingq-tool known-import <word-list.txt> [db-path]"
    , "  zeit-lingq-tool known-info [db-path]"
    , "  zeit-lingq-tool known-compute [db-path]"
    , "  zeit-lingq-tool lingq-upload <article-id> [db-path]"
    , ""
    , "Set ZEIT_COOKIE to pass an authenticated zeit.de cookie header for paid articles."
    , "Set LINGQ_API_KEY, and optionally LINGQ_COLLECTION_ID, before uploading to LingQ."
    ]

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label raw =
  case reads raw of
    [(value, "")] | value > 0 -> Right value
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)
