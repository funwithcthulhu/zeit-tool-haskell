{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Time (fromGregorian)
import Data.Map.Strict qualified as Map
import Control.Monad (when)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Update
import ZeitLingq.Cli
import ZeitLingq.Core.Batch
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Core.Upload
import ZeitLingq.Domain.Article
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Settings
import ZeitLingq.Infrastructure.Audio
import ZeitLingq.Infrastructure.Sqlite
import ZeitLingq.Infrastructure.Lingq
import ZeitLingq.Infrastructure.Zeit
import ZeitLingq.Ports (SettingsPort(..))
import ZeitLingq.Text.German

main :: IO ()
main = hspec $ do
  describe "German tokenization and stemming" $ do
    it "keeps umlauted words and strips punctuation" $ do
      tokenizeGerman "Häuser, laufen! und ß." `shouldBe` ["häuser", "laufen", "und"]

    it "normalizes common plural variants into ASCII stems" $ do
      stemGerman "Häuser" `shouldBe` "haus"

  describe "Known-word estimation" $ do
    it "imports, stems and scores vocabulary" $ do
      let known = importKnownWordStems "laufen\nHaus\n"
      estimateKnownPct known "Das Haus laufen langsam" `shouldBe` Just 50

    it "returns Nothing for empty article text" $ do
      estimateKnownPct Set.empty "" `shouldBe` Nothing

  describe "Article helpers" $ do
    it "adds a date prefix once" $ do
      lessonTitle (fromGregorian 2026 5 2) True "Titel" `shouldBe` "2026-05-02 - Titel"
      lessonTitle (fromGregorian 2026 5 2) True "2026-05-01 - Titel" `shouldBe` "2026-05-01 - Titel"

    it "applies fetch filters like the Electron app" $ do
      applyWordFilter (WordFilter (Just 5) (Just 10)) demoArticle `shouldBe` SkipBelowMinimum 4 5

  describe "Pure app update" $ do
    it "navigates to the article view and persists it" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 7)
              , summaryUrl = "https://example.com"
              , summaryTitle = "Test"
              , summarySection = "Wissen"
              , summaryWordCount = 123
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          (nextModel, commands) = update (ArticleOpened summary) initialModel
      currentView nextModel `shouldBe` ArticleView
      selectedArticle nextModel `shouldBe` Just summary
      commands `shouldBe` [PersistCurrentView ArticleView]

  describe "CLI argument parsing" $ do
    it "defaults to the demo command" $ do
      parseArgs [] `shouldBe` Right ShowDemo

    it "parses browse, fetch and library commands" $ do
      parseArgs ["browse", "wissen", "2"] `shouldBe` Right (BrowseZeit "wissen" 2)
      parseArgs ["fetch", "https://www.zeit.de/wissen/2026-05/beispiel"] `shouldBe` Right (FetchArticle "https://www.zeit.de/wissen/2026-05/beispiel" defaultDbPath)
      parseArgs ["batch-fetch", "urls.txt", "custom.db", "500", "2000"] `shouldBe` Right (BatchFetch "urls.txt" "custom.db" (WordFilter (Just 500) (Just 2000)))
      parseArgs ["library", "custom.db"] `shouldBe` Right (ShowLibrary "custom.db")
      parseArgs ["known-import", "words.txt", "custom.db"] `shouldBe` Right (ImportKnownWords "words.txt" "custom.db")
      parseArgs ["known-info"] `shouldBe` Right (KnownWordsInfo defaultDbPath)
      parseArgs ["known-compute", "custom.db"] `shouldBe` Right (ComputeKnownPct "custom.db")
      parseArgs ["lingq-upload", "42", "custom.db"] `shouldBe` Right (UploadLingq 42 "custom.db")
      parseArgs ["audio-download", "42", "audio-cache", "custom.db"] `shouldBe` Right (DownloadAudio 42 "audio-cache" "custom.db")

  describe "Batch fetch use case" $ do
    it "saves successful articles and skips articles outside the word filter" $ do
      let fetcher url
            | url == "bad" = pure (Left "network")
            | url == "short" = pure (Right demoArticle {articleUrl = url, articleParagraphs = ["eins zwei"]})
            | otherwise = pure (Right demoArticle {articleUrl = url})
          saver _ = pure (ArticleId 42)
      results <- batchFetchArticles fetcher saver (WordFilter (Just 3) Nothing) ["ok", "short", "bad"]
      results
        `shouldBe` [ BatchSaved "ok" (ArticleId 42)
                   , BatchSkipped "short" (SkipBelowMinimum 2 3)
                   , BatchFailed "bad" "network"
                   ]

  describe "Batch upload use case" $ do
    it "chooses section-specific collections and marks tracked articles" $ do
      let config =
            BatchUploadConfig
              { uploadLanguageCode = "de"
              , uploadFallbackCollection = Just "fallback"
              , uploadSectionCollections = Map.fromList [("Wissen", "wissen-course")]
              , uploadDatePrefixEnabled = True
              , uploadDay = fromGregorian 2026 5 2
              }
          uploader lang collection article =
            pure (Right (LingqLesson (lang <> ":" <> maybe "none" id collection) ("lesson:" <> articleTitle article)))
          marker _ _ =
            pure ()
      results <- batchUploadArticles uploader marker config [demoArticle {articleId = Just (ArticleId 3)}]
      results
        `shouldBe` [ UploadSucceeded
                       (ArticleId 3)
                       "2026-05-02 - Demo"
                       (LingqLesson "de:wissen-course" "lesson:2026-05-02 - Demo")
                   ]

    it "returns upload failures without marking them" $ do
      let config =
            BatchUploadConfig
              { uploadLanguageCode = "de"
              , uploadFallbackCollection = Nothing
              , uploadSectionCollections = Map.empty
              , uploadDatePrefixEnabled = False
              , uploadDay = fromGregorian 2026 5 2
              }
          uploader _ _ _ = pure (Left "upload failed")
          marker _ _ = expectationFailure "marker should not be called"
      results <- batchUploadArticles uploader marker config [demoArticle {articleId = Just (ArticleId 4)}]
      results `shouldBe` [UploadFailed (Just (ArticleId 4)) "Demo" "upload failed"]

  describe "SQLite library adapter" $ do
    it "saves and reloads articles with summaries and stats" $ do
      withLibrary ":memory:" $ \db -> do
        savedId <- saveArticleSqlite db demoArticle
        loaded <- getArticleSqlite db savedId
        fmap articleTitle loaded `shouldBe` Just "Demo"
        fmap articleParagraphs loaded `shouldBe` Just ["eins zwei", "drei vier"]

        summaries <- getArticlesSqlite db (WordFilter Nothing Nothing)
        map summaryTitle summaries `shouldBe` ["Demo"]
        map summaryWordCount summaries `shouldBe` [4]

        stats <- getStatsSqlite db
        totalArticles stats `shouldBe` 1
        averageWordCount stats `shouldBe` 4

    it "preserves upload and ignored state when an article is refetched" $ do
      withLibrary ":memory:" $ \db -> do
        savedId <- saveArticleSqlite db demoArticle
        markUploadedSqlite db savedId (LingqLesson "lesson-1" "https://lingq.example/lesson-1")
        setIgnoredSqlite db savedId True

        _ <- saveArticleSqlite db demoArticle {articleTitle = "Demo refetched"}
        loaded <- getArticleSqlite db savedId

        fmap articleTitle loaded `shouldBe` Just "Demo refetched"
        fmap articleIgnored loaded `shouldBe` Just True
        fmap articleUploadedLesson loaded `shouldBe` Just (Just (LingqLesson "lesson-1" "https://lingq.example/lesson-1"))

    it "stores known-word stems and computes cached known percentages" $ do
      withLibrary ":memory:" $ \db -> do
        savedId <- saveArticleSqlite db demoArticle
        added <- saveKnownWordsSqlite db "de" (importKnownWordStems "eins\ndrei")
        added `shouldBe` 2

        getKnownStemCountSqlite db "de" `shouldReturn` 2
        computeKnownPctSqlite db "de" `shouldReturn` Right 1

        loaded <- getArticleSqlite db savedId
        fmap articleKnownPct loaded `shouldBe` Just (Just 50)

        clearAllKnownPctSqlite db
        reloaded <- getArticleSqlite db savedId
        fmap articleKnownPct reloaded `shouldBe` Just Nothing

        clearKnownWordsSqlite db "de"
        getKnownStemCountSqlite db "de" `shouldReturn` 0

    it "updates audio metadata for saved articles" $ do
      withLibrary ":memory:" $ \db -> do
        savedId <- saveArticleSqlite db demoArticle
        setAudioUrlSqlite db savedId (Just "https://cdn.example/audio.m4a")
        setAudioPathSqlite db savedId (Just "C:\\audio\\demo.m4a")
        loaded <- getArticleSqlite db savedId
        fmap articleAudioUrl loaded `shouldBe` Just (Just "https://cdn.example/audio.m4a")
        fmap articleAudioPath loaded `shouldBe` Just (Just "C:\\audio\\demo.m4a")

  describe "JSON settings adapter" $ do
    it "returns defaults when the settings file does not exist" $ do
      withTempSettingsPath $ \path -> do
        loadSettings path `shouldReturn` defaultSettings

    it "persists the settings port values independently" $ do
      withTempSettingsPath $ \path -> do
        let port = jsonSettingsPort path
        saveCurrentView port LingqView
        saveDatePrefixEnabled port False
        saveSectionCollections port (Map.fromList [("Wissen", "course-1")])

        loadCurrentView port `shouldReturn` LingqView
        loadDatePrefixEnabled port `shouldReturn` False
        loadSectionCollections port `shouldReturn` Map.fromList [("Wissen", "course-1")]

  describe "LingQ adapter helpers" $ do
    it "normalizes lesson text without flattening paragraphs" $ do
      normalizeLessonText " eins   zwei\nnoch \n\n  drei\tvier " `shouldBe` "eins zwei noch\n\ndrei vier"

    it "parses collection responses with numeric ids" $ do
      let value = decodeValue "{\"results\":[{\"id\":12,\"title\":\"Wissen\",\"lessons_count\":3}]}"
      parseCollectionsValue value `shouldBe` Right [LingqCollection "12" "Wissen" 3]

    it "extracts known-word terms from paged responses" $ do
      let value = decodeValue "{\"results\":[{\"term\":\"Haus\"},{\"word\":\"laufen\"},{\"text\":\"  Leer  \"}]}"
      parseKnownWordTerms value `shouldBe` ["haus", "laufen", "leer"]

  describe "Audio helpers" $ do
    it "builds stable safe filenames for article audio" $ do
      let article =
            demoArticle
              { articleId = Just (ArticleId 12)
              , articleTitle = "Hören: Häuser & Sachen!"
              , articleAudioUrl = Just "https://cdn.example/audio/clip.m4a?token=abc"
              }
      audioFilename article `shouldBe` "12-h-ren-h-user-sachen.m4a"

  describe "Zeit HTML extraction" $ do
    it "extracts article links from index markup" $ do
      let articles =
            extractArticleList
              "Wissen"
              "<main><a href=\"/wissen/2026-05/beispiel\">Ein ziemlich langer Titel</a><a href=\"/wissen/2026-05/beispiel?utm=x\">Ein ziemlich langer Titel</a><a href=\"/abo\">Abo</a></main>"
      map summaryUrl articles `shouldBe` ["https://www.zeit.de/wissen/2026-05/beispiel"]
      map summaryTitle articles `shouldBe` ["Ein ziemlich langer Titel"]

    it "extracts article content from article markup" $ do
      let html =
            "<html><head><title>Fallback | ZEIT ONLINE</title><meta name=\"author\" content=\"Ada\"><meta property=\"article:published_time\" content=\"2026-05-02\"><meta property=\"article:section\" content=\"Wissen\"></head><body><article><h1>Der Haskell-Test</h1><h2>Abschnitt</h2><p>Das ist ein Absatz mit genug Worten fuer den Parser.</p><p>Noch ein Absatz mit sauberem Text und Inhalt.</p></article></body></html>"
      case extractArticleContent "https://www.zeit.de/wissen/2026-05/haskell-test" html of
        Left err -> expectationFailure (show err)
        Right article -> do
          articleTitle article `shouldBe` "Der Haskell-Test"
          articleAuthor article `shouldBe` "Ada"
          articleSection article `shouldBe` "Wissen"
          articleParagraphs article `shouldBe` ["## Abschnitt", "Das ist ein Absatz mit genug Worten fuer den Parser.", "Noch ein Absatz mit sauberem Text und Inhalt."]

demoArticle :: Article
demoArticle =
  Article
    { articleId = Nothing
    , articleUrl = "https://example.com"
    , articleTitle = "Demo"
    , articleSubtitle = ""
    , articleAuthor = ""
    , articleDate = Nothing
    , articleSection = "Wissen"
    , articleParagraphs = ["eins zwei", "drei vier"]
    , articleFetchedAt = Nothing
    , articleUploadedLesson = Nothing
    , articleIgnored = False
    , articleAudioUrl = Nothing
    , articleAudioPath = Nothing
    , articleKnownPct = Nothing
    }

withTempSettingsPath :: (FilePath -> IO a) -> IO a
withTempSettingsPath action = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "zeit-tool-settings-test.json"
  existsBefore <- doesFileExist path
  when existsBefore (removeFile path)
  result <- action path
  stillExists <- doesFileExist path
  when stillExists (removeFile path)
  pure result

decodeValue :: BL.ByteString -> Value
decodeValue raw =
  case eitherDecode raw of
    Right value -> value
    Left err -> error err
