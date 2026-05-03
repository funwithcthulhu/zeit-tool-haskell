{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (fromGregorian, getCurrentTime)
import System.Environment (getArgs, lookupEnv)
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Update (Event(..), update)
import ZeitLingq.Cli
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Domain.Article (composeCleanText, lessonTitle, wordCount)
import ZeitLingq.Domain.Section (allSections)
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Sqlite
import ZeitLingq.Infrastructure.Zeit

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> putStr err
    Right command -> runCommand command

runCommand :: CliCommand -> IO ()
runCommand ShowDemo = runDemo
runCommand ListSections =
  for_ allSections $ \section ->
    putStrLn (T.unpack (sectionId section <> "\t" <> sectionLabel section))
runCommand (BrowseZeit sectionIdent page) = do
  session <- sessionFromEnv
  result <- fetchArticleListZeit session sectionIdent page
  case result of
    Left err -> print err
    Right articles ->
      for_ articles $ \article ->
        putStrLn (T.unpack (summaryTitle article <> "\n  " <> summaryUrl article))
runCommand (FetchArticle url dbPath) = do
  session <- sessionFromEnv
  result <- fetchArticleContentZeit session url
  case result of
    Left err -> print err
    Right article ->
      withLibrary dbPath $ \db -> do
        savedId <- saveArticleSqlite db article
        putStrLn ("Saved article " <> show (unArticleId savedId) <> ": " <> T.unpack (articleTitle article))
runCommand (ShowLibrary dbPath) =
  withLibrary dbPath $ \db -> do
    articles <- getArticlesSqlite db (WordFilter Nothing Nothing)
    if null articles
      then putStrLn "No saved articles."
      else
        for_ articles $ \article ->
          putStrLn (showSummary article)
runCommand (ImportKnownWords sourcePath dbPath) = do
  rawWords <- TIO.readFile sourcePath
  let stems = importKnownWordStems rawWords
  withLibrary dbPath $ \db -> do
    count <- saveKnownWordsSqlite db "de" stems
    clearAllKnownPctSqlite db
    putStrLn ("Imported " <> show count <> " known German stems.")
runCommand (KnownWordsInfo dbPath) =
  withLibrary dbPath $ \db -> do
    count <- getKnownStemCountSqlite db "de"
    syncedAt <- getKnownWordsSyncedAtSqlite db "de"
    putStrLn ("Known German stems: " <> show count)
    putStrLn ("Synced at: " <> maybe "-" show syncedAt)
runCommand (ComputeKnownPct dbPath) =
  withLibrary dbPath $ \db -> do
    result <- computeKnownPctSqlite db "de"
    case result of
      Left err -> putStrLn (T.unpack err)
      Right count -> putStrLn ("Updated known-word estimates for " <> show count <> " articles.")

sessionFromEnv :: IO ZeitSession
sessionFromEnv = do
  cookie <- lookupEnv "ZEIT_COOKIE"
  pure (ZeitSession (maybe "" T.pack cookie))

showSummary :: ArticleSummary -> String
showSummary article =
  maybe "-" (show . unArticleId) (summaryId article)
    <> "\t"
    <> show (summaryWordCount article)
    <> " words\t"
    <> T.unpack (summaryTitle article)

runDemo :: IO ()
runDemo = do
  now <- getCurrentTime
  let demoArticle =
        Article
          { articleId = Just (ArticleId 1)
          , articleUrl = "https://www.zeit.de/wissen/2026-05/beispiel"
          , articleTitle = "Beispielartikel"
          , articleSubtitle = "Ein Haskell-first Umbau"
          , articleAuthor = "Codex"
          , articleDate = Just "2026-05-02"
          , articleSection = "Wissen"
          , articleParagraphs =
              [ "Das ist ein kurzer Beispieltext fur die erste Migrationsstufe."
              , "Wir portieren zuerst das reine Kernverhalten und hangen danach eine GUI an."
              ]
          , articleFetchedAt = Just now
          , articleUploadedLesson = Nothing
          , articleIgnored = False
          , articleAudioUrl = Nothing
          , articleAudioPath = Nothing
          , articleKnownPct = Nothing
          }
      demoDay = fromGregorian 2026 5 2
      knownWords = importKnownWordStems "das\nkurz\ntext\ngui\nhangen"
      knownPct = estimateKnownPct knownWords (composeCleanText demoArticle)
      (nextModel, commands) =
        update
          (SectionCollectionsChanged (Map.fromList [("Wissen", "science-collection")]))
          initialModel

  putStrLn "Zeit LingQ Haskell migration scaffold"
  putStrLn "==================================="
  putStrLn ("Sections ported: " <> show (length allSections))
  putStrLn ("Demo article word count: " <> show (wordCount demoArticle))
  putStrLn ("Known-word estimate: " <> show knownPct)
  putStrLn ("LingQ lesson title: " <> T.unpack (lessonTitle demoDay True (articleTitle demoArticle)))
  putStrLn ("Clean text preview: " <> take 80 (T.unpack (composeCleanText demoArticle)) <> "...")
  putStrLn ("Current view in pure model: " <> show (currentView nextModel))
  putStrLn ("Pending pure commands: " <> show commands)
