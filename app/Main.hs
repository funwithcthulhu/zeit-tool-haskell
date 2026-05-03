{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (fromGregorian, getCurrentTime, utctDay)
import System.Environment (getArgs, lookupEnv)
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.App.Update (Event(..), update)
import ZeitLingq.Cli
import ZeitLingq.Core.Batch
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Core.Upload
import ZeitLingq.Domain.Article (composeCleanText, lessonTitle, wordCount)
import ZeitLingq.Domain.Section (allSections)
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Sqlite
import ZeitLingq.Infrastructure.Audio
import ZeitLingq.Infrastructure.Lingq
import ZeitLingq.Infrastructure.Settings
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
runCommand (BatchFetch sourcePath dbPath filters) = do
  rawUrls <- TIO.readFile sourcePath
  session <- sessionFromEnv
  let urls = filter (not . T.null) (map T.strip (T.lines rawUrls))
      fetcher url = firstTextZeit <$> fetchArticleContentZeit session url
  withLibrary dbPath $ \db -> do
    results <- batchFetchArticles fetcher (saveArticleSqlite db) filters urls
    for_ results print
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
runCommand (UploadLingq ident dbPath settingsPath) = do
  maybeApiKey <- lookupEnv "LINGQ_API_KEY"
  case maybeApiKey of
    Nothing -> putStrLn "Set LINGQ_API_KEY before uploading to LingQ."
    Just apiKey -> do
      maybeCollection <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
      now <- getCurrentTime
      withLibrary dbPath $ \db -> do
        maybeArticle <- getArticleSqlite db (ArticleId ident)
        case maybeArticle of
          Nothing -> putStrLn ("Article not found: " <> show ident)
          Just article -> do
            settings <- loadSettings settingsPath
            let token = LingqToken (T.pack apiKey)
                config =
                  uploadConfigFromPreferences
                    (utctDay now)
                    maybeCollection
                    (settingsDatePrefixEnabled settings)
                    (settingsSectionCollections settings)
                uploader lang collection titledArticle =
                  firstText <$> uploadLessonLingq token lang collection titledArticle
                marker = markUploadedSqlite db
            results <- batchUploadArticles uploader marker config [article]
            for_ results print
runCommand (DownloadAudio ident audioDir dbPath) =
  withLibrary dbPath $ \db -> do
    maybeArticle <- getArticleSqlite db (ArticleId ident)
    case maybeArticle of
      Nothing -> putStrLn ("Article not found: " <> show ident)
      Just article -> do
        result <- downloadArticleAudio audioDir article
        case result of
          Left err -> print err
          Right path -> do
            setAudioPathSqlite db (ArticleId ident) (Just path)
            putStrLn ("Saved audio: " <> path)
runCommand (IgnoreUrl url dbPath) =
  withLibrary dbPath $ \db -> do
    ignoreUrlSqlite db url
    putStrLn ("Ignored: " <> T.unpack url)
runCommand (UnignoreUrl url dbPath) =
  withLibrary dbPath $ \db -> do
    unignoreUrlSqlite db url
    putStrLn ("Unignored: " <> T.unpack url)
runCommand (ListIgnored dbPath) =
  withLibrary dbPath $ \db -> do
    urls <- getIgnoredUrlsSqlite db
    if null urls
      then putStrLn "No ignored URLs."
      else for_ urls (putStrLn . T.unpack)
runCommand (ShowSettings settingsPath) =
  loadSettings settingsPath >>= TIO.putStr . formatSettings
runCommand (SetSettingsView view settingsPath) = do
  _ <- updateSettingsFile settingsPath $ \settings ->
    settings {settingsCurrentView = view}
  putStrLn ("Saved current view: " <> T.unpack (viewToText view))
runCommand (SetSettingsBrowseSection sectionId settingsPath) = do
  _ <- updateSettingsFile settingsPath $ \settings ->
    settings {settingsBrowseSection = sectionId}
  putStrLn ("Saved browse section: " <> T.unpack sectionId)
runCommand (SetSettingsDatePrefix enabled settingsPath) = do
  _ <- updateSettingsFile settingsPath $ \settings ->
    settings {settingsDatePrefixEnabled = enabled}
  putStrLn ("Saved date prefix: " <> T.unpack (boolText enabled))
runCommand (SetSettingsSectionCollection sectionName collectionId settingsPath) = do
  _ <- updateSettingsFile settingsPath $ \settings ->
    settings
      { settingsSectionCollections =
          Map.insert sectionName collectionId (settingsSectionCollections settings)
      }
  putStrLn ("Mapped " <> T.unpack sectionName <> " to LingQ collection " <> T.unpack collectionId)
runCommand (ClearSettingsSectionCollection sectionName settingsPath) = do
  _ <- updateSettingsFile settingsPath $ \settings ->
    settings
      { settingsSectionCollections =
          Map.delete sectionName (settingsSectionCollections settings)
      }
  putStrLn ("Cleared LingQ collection mapping for " <> T.unpack sectionName)

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

firstText :: Either LingqError LingqLesson -> Either T.Text LingqLesson
firstText (Right lesson) = Right lesson
firstText (Left err) = Left (T.pack (show err))

firstTextZeit :: Either ZeitError Article -> Either T.Text Article
firstTextZeit (Right article) = Right article
firstTextZeit (Left err) = Left (T.pack (show err))

formatSettings :: Settings -> T.Text
formatSettings settings =
  T.unlines
    ( [ "currentView: " <> viewToText (settingsCurrentView settings)
      , "browseSection: " <> settingsBrowseSection settings
      , "datePrefixEnabled: " <> boolText (settingsDatePrefixEnabled settings)
      , "sectionCollections:"
      ]
        <> collectionLines
    )
  where
    mappings = Map.toList (settingsSectionCollections settings)
    collectionLines
      | null mappings = ["  -"]
      | otherwise = map formatCollection mappings

formatCollection :: (T.Text, T.Text) -> T.Text
formatCollection (sectionName, collectionId) =
  "  " <> sectionName <> ": " <> collectionId

boolText :: Bool -> T.Text
boolText True = "on"
boolText False = "off"

updateSettingsFile :: FilePath -> (Settings -> Settings) -> IO Settings
updateSettingsFile settingsPath change = do
  settings <- loadSettings settingsPath
  let nextSettings = change settings
  saveSettings settingsPath nextSettings
  pure nextSettings

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
