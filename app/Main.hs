{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (addUTCTime, getCurrentTime, utctDay)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.Cli
import ZeitLingq.Core.Batch
import ZeitLingq.Core.Browse
import ZeitLingq.Core.KnownWords (importKnownWordStems)
import ZeitLingq.Core.Upload
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
    Left err -> hPutStr stderr err >> exitFailure
    Right command -> runCommand command

runCommand :: CliCommand -> IO ()
runCommand ShowHelp = putStr usageText
runCommand ListSections =
  for_ allSections $ \section ->
    putStrLn (T.unpack (sectionId section <> "\t" <> sectionLabel section))
runCommand (BrowseZeit sectionIdent page dbPath) = do
  session <- sessionFromEnv
  result <- fetchArticleListZeit session sectionIdent page
  case result of
    Left err -> print err
    Right articles ->
      withLibrary dbPath $ \db -> do
        ignoredUrls <- Set.fromList <$> getIgnoredUrlsSqlite db
        for_ (hideIgnoredSummaries ignoredUrls articles) $ \article ->
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
runCommand (ShowStats dbPath) =
  withLibrary dbPath $ \db ->
    getStatsSqlite db >>= TIO.putStr . formatStats
runCommand (DeleteArticle ident dbPath) =
  withLibrary dbPath $ \db ->
    withExistingArticle db ident $ \articleIdValue -> do
      deleteArticleSqlite db articleIdValue
      putStrLn ("Deleted article " <> show ident)
runCommand (DeleteOlderThan days onlyUploaded onlyUnuploaded dbPath) = do
  now <- getCurrentTime
  let cutoff = addUTCTime (negate (fromIntegral days * 86400)) now
  withLibrary dbPath $ \db -> do
    deleted <- deleteOlderThanSqlite db cutoff onlyUploaded onlyUnuploaded
    putStrLn ("Deleted " <> show deleted <> " article(s).")
runCommand (DeleteIgnored dbPath) =
  withLibrary dbPath $ \db -> do
    deleted <- deleteIgnoredSqlite db
    putStrLn ("Deleted " <> show deleted <> " ignored article(s).")
runCommand (IgnoreArticle ident dbPath) =
  withLibrary dbPath $ \db ->
    withExistingArticle db ident $ \articleIdValue -> do
      setIgnoredSqlite db articleIdValue True
      putStrLn ("Ignored article " <> show ident)
runCommand (UnignoreArticle ident dbPath) =
  withLibrary dbPath $ \db ->
    withExistingArticle db ident $ \articleIdValue -> do
      setIgnoredSqlite db articleIdValue False
      putStrLn ("Unignored article " <> show ident)
runCommand (SyncKnownWords dbPath) = do
  maybeApiKey <- lookupEnv "LINGQ_API_KEY"
  case maybeApiKey of
    Nothing -> putStrLn "Set LINGQ_API_KEY before syncing known words from LingQ."
    Just apiKey -> do
      result <- fetchKnownWordsLingq (LingqToken (T.pack apiKey)) "de"
      case result of
        Left err -> print err
        Right terms ->
          withLibrary dbPath $ \db -> do
            let stems = importKnownWordStems (T.unlines terms)
            count <- saveKnownWordsSqlite db "de" stems
            computeResult <- computeKnownPctSqlite db "de"
            putStrLn ("Synced " <> show count <> " known German stems from LingQ.")
            case computeResult of
              Left err -> putStrLn (T.unpack err)
              Right articleCount ->
                putStrLn ("Updated known-word estimates for " <> show articleCount <> " articles.")
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
                    (settingsLingqLanguage settings)
                    maybeCollection
                    (settingsDatePrefixEnabled settings)
                    (settingsSectionCollections settings)
                uploader lang collection titledArticle =
                  firstText <$> uploadLessonLingq token lang collection titledArticle
                updater lang existingLesson titledArticle =
                  firstText <$> updateLessonLingq token lang existingLesson titledArticle
                marker = markUploadedSqlite db
            results <- batchUploadArticles uploader updater marker config [article]
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
  userAgent <- lookupEnv "ZEIT_USER_AGENT"
  pure
    ( ZeitSession
        (maybe "" T.pack cookie)
        (maybe defaultZeitUserAgent T.pack userAgent)
    )

showSummary :: ArticleSummary -> String
showSummary article =
  maybe "-" (show . unArticleId) (summaryId article)
    <> "\t"
    <> show (summaryWordCount article)
    <> " words\t"
    <> T.unpack (summaryTitle article)

formatStats :: LibraryStats -> T.Text
formatStats stats =
  T.unlines
    ( [ "articles: " <> tshow (totalArticles stats)
      , "uploaded: " <> tshow (uploadedArticles stats)
      , "averageWords: " <> tshow (averageWordCount stats)
      , "sections:"
      ]
        <> sectionLines
    )
  where
    sections = Map.toList (sectionCounts stats)
    sectionLines
      | null sections = ["  -"]
      | otherwise =
          map
            (\(sectionName, total) -> "  " <> sectionName <> ": " <> tshow total)
            sections

withExistingArticle :: LibraryDb -> Int -> (ArticleId -> IO ()) -> IO ()
withExistingArticle db ident action = do
  let articleIdValue = ArticleId ident
  article <- getArticleSqlite db articleIdValue
  case article of
    Nothing -> putStrLn ("Article not found: " <> show ident)
    Just _ -> action articleIdValue

tshow :: Show a => a -> T.Text
tshow = T.pack . show

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
      , "uiTheme: " <> uiThemeToText (settingsUiTheme settings)
      , "zeitUserAgent: " <> settingsZeitUserAgent settings
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
