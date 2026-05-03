{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Time (fromGregorian)
import Data.Map.Strict qualified as Map
import Control.Monad (when)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Update
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Domain.Article
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Settings
import ZeitLingq.Infrastructure.Sqlite
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
