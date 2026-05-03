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
    , ""
    , "Set ZEIT_COOKIE to pass an authenticated zeit.de cookie header for paid articles."
    ]

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label raw =
  case reads raw of
    [(value, "")] | value > 0 -> Right value
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)
