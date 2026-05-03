{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Time (fromGregorian)
import Data.Map.Strict qualified as Map
import Control.Monad (when)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Functor.Identity (Identity(..), runIdentity)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import ZeitLingq.App.Driver
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Runtime qualified as Runtime
import ZeitLingq.App.Startup
import ZeitLingq.App.UploadConfig
import ZeitLingq.App.Update
import ZeitLingq.App.ViewModel
import ZeitLingq.Cli
import ZeitLingq.Core.Batch
import ZeitLingq.Core.Browse
import ZeitLingq.Core.KnownWords (estimateKnownPct, importKnownWordStems)
import ZeitLingq.Core.Upload
import ZeitLingq.Domain.Article
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Settings
import ZeitLingq.Infrastructure.Audio
import ZeitLingq.Infrastructure.Sqlite
import ZeitLingq.Infrastructure.Lingq
import ZeitLingq.Infrastructure.Zeit
import ZeitLingq.Ports (AppPorts(..), LibraryPort(..), LingqPort(..), SettingsPort(..), ZeitPort(..))
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

    it "applies fetch filters" $ do
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
      selectedArticleContent nextModel `shouldBe` Nothing
      commands `shouldBe` [PersistCurrentView ArticleView, LoadArticle (ArticleId 7)]

    it "stores loaded article content and clears it when closing" $ do
      let (loadedModel, loadedCommands) = update (ArticleContentLoaded demoArticle) initialModel
          (closedModel, closedCommands) = update ArticleClosed loadedModel
      selectedArticleContent loadedModel `shouldBe` Just demoArticle
      loadedCommands `shouldBe` []
      selectedArticleContent closedModel `shouldBe` Nothing
      currentView closedModel `shouldBe` LibraryView
      closedCommands `shouldBe` [PersistCurrentView LibraryView]

    it "hydrates startup model from the settings port" $ do
      let port =
            SettingsPort
              { loadCurrentView = Identity LingqView
              , saveCurrentView = \_ -> Identity ()
              , loadBrowseSection = Identity "wissen"
              , saveBrowseSection = \_ -> Identity ()
              , loadDatePrefixEnabled = Identity False
              , saveDatePrefixEnabled = \_ -> Identity ()
              , loadSectionCollections = Identity (Map.fromList [("Wissen", "course-1")])
              , saveSectionCollections = \_ -> Identity ()
              }
          model = runIdentity (loadInitialModel port)
      currentView model `shouldBe` LingqView
      browseSectionId model `shouldBe` "wissen"
      datePrefixEnabled model `shouldBe` False
      sectionCollections model `shouldBe` Map.fromList [("Wissen", "course-1")]

    it "stores loaded article lists for GUI screens" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 11)
              , summaryUrl = "https://example.com/list"
              , summaryTitle = "List Item"
              , summarySection = "Wissen"
              , summaryWordCount = 432
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          (browseModel, browseCommands) = update (BrowseArticlesLoaded [summary]) initialModel
          (libraryModel, libraryCommands) = update (LibraryArticlesLoaded [summary]) initialModel
          (lingqModel, lingqCommands) = update (LingqArticlesLoaded [summary]) initialModel
      browseArticles browseModel `shouldBe` [summary]
      libraryArticles libraryModel `shouldBe` [summary]
      lingqArticles lingqModel `shouldBe` [summary]
      browseCommands <> libraryCommands <> lingqCommands `shouldBe` []

    it "emits refresh commands with screen context" $ do
      let filters = WordFilter (Just 400) (Just 1200)
          browseModel = initialModel {browseSectionId = "wissen"}
      snd (update (BrowseSectionSelected "kultur") initialModel)
        `shouldBe` [PersistBrowseSection "kultur", RefreshBrowse "kultur" 1]
      snd (update (BrowseFilterChanged filters) browseModel)
        `shouldBe` [RefreshBrowse "wissen" 1]
      snd (update (LibraryFilterChanged filters) initialModel)
        `shouldBe` [RefreshLibrary filters]
      snd (update (LingqFilterChanged filters) initialModel)
        `shouldBe` [RefreshLingqLibrary filters]

  describe "App command runtime" $ do
    it "turns refresh commands into loaded events" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 12)
              , summaryUrl = "https://example.com/runtime"
              , summaryTitle = "Runtime Item"
              , summarySection = "Wissen"
              , summaryWordCount = 555
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ports = testPorts summary
          filters = WordFilter Nothing Nothing
      runIdentity (Runtime.runCommand ports (RefreshBrowse "kultur" 2))
        `shouldBe` [BrowseArticlesLoaded [summary {summarySection = "kultur"}]]
      runIdentity (Runtime.runCommand ports (RefreshLibrary filters))
        `shouldBe` [LibraryArticlesLoaded [summary]]
      runIdentity (Runtime.runCommand ports (RefreshLingqLibrary filters))
        `shouldBe` [LingqArticlesLoaded [summary]]
      runIdentity (Runtime.runCommand ports (LoadArticle (ArticleId 1)))
        `shouldBe` [ArticleContentLoaded demoArticle]

    it "reports a missing article when content cannot be loaded" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 404)
              , summaryUrl = "https://example.com/missing"
              , summaryTitle = "Missing"
              , summarySection = "Wissen"
              , summaryWordCount = 100
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports = basePorts {libraryPort = (libraryPort basePorts) {loadArticle = \_ -> Identity Nothing}}
      runIdentity (Runtime.runCommand ports (LoadArticle (ArticleId 404)))
        `shouldBe` [Notify ErrorNotice "Article not found."]

  describe "App event driver" $ do
    it "dispatches refresh commands and folds loaded events back into the model" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 21)
              , summaryUrl = "https://example.com/driver"
              , summaryTitle = "Driver Item"
              , summarySection = "Wissen"
              , summaryWordCount = 777
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          model = runIdentity (dispatchEvent (testPorts summary) initialModel (BrowseSectionSelected "kultur"))
      browseSectionId model `shouldBe` "kultur"
      browseArticles model `shouldBe` [summary {summarySection = "kultur"}]

    it "dispatches article-open commands and folds loaded content into the model" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/open"
              , summaryTitle = "Open Me"
              , summarySection = "Wissen"
              , summaryWordCount = 777
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          model = runIdentity (dispatchEvent (testPorts summary) initialModel (ArticleOpened summary))
      currentView model `shouldBe` ArticleView
      selectedArticle model `shouldBe` Just summary
      selectedArticleContent model `shouldBe` Just demoArticle

  describe "Pure app view model" $ do
    it "projects navigation and status badges for a GUI adapter" $ do
      let model =
            initialModel
              { currentView = LibraryView
              , zeitStatus = AuthStatus True (Just "paid session")
              , lingqStatus = AuthStatus False Nothing
              , libraryFilter = WordFilter (Just 500) (Just 1500)
              , datePrefixEnabled = False
              }
          viewModel = appViewModel model
      vmTitle viewModel `shouldBe` "Saved Articles"
      map navLabel (vmNavItems viewModel) `shouldBe` ["Browse", "Library", "LingQ", "Zeit"]
      map navActive (vmNavItems viewModel) `shouldBe` [False, True, False, False]
      vmStatusBadges viewModel
        `shouldBe` [ StatusBadge "Zeit" "paid session" True
                   , StatusBadge "LingQ" "disconnected" False
                   ]
      vmActiveFilter viewModel `shouldBe` "500-1500 words"
      vmDatePrefix viewModel `shouldBe` "Date prefix: off"

    it "projects article summaries into display rows" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 9)
              , summaryUrl = "https://example.com"
              , summaryTitle = "Der Test"
              , summarySection = "Wissen"
              , summaryWordCount = 812
              , summaryIgnored = False
              , summaryUploaded = True
              , summaryKnownPct = Just 73
              }
      articleRowView summary
        `shouldBe` ArticleRowView
          { rowId = Just (ArticleId 9)
          , rowTitle = "Der Test"
          , rowMeta = "Wissen | 812 words"
          , rowKnownPct = "known: 73%"
          , rowUploadStatus = "uploaded"
          }

    it "projects current screen rows" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 13)
              , summaryUrl = "https://example.com/screen"
              , summaryTitle = "Screen Row"
              , summarySection = "Kultur"
              , summaryWordCount = 640
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          viewModel =
            appViewModel
              initialModel
                { currentView = BrowseView
                , browseArticles = [summary]
                }
      map rowTitle (vmArticleRows viewModel) `shouldBe` ["Screen Row"]

    it "projects loaded article content paragraphs" $ do
      let viewModel =
            appViewModel
              initialModel
                { currentView = ArticleView
                , selectedArticleContent = Just demoArticle
                }
      vmSelectedArticleParagraphs viewModel `shouldBe` ["eins zwei", "drei vier"]

  describe "CLI argument parsing" $ do
    it "defaults to the demo command" $ do
      parseArgs [] `shouldBe` Right ShowDemo

    it "parses browse, fetch and library commands" $ do
      parseArgs ["browse", "wissen", "2"] `shouldBe` Right (BrowseZeit "wissen" 2 defaultDbPath)
      parseArgs ["browse", "wissen", "2", "custom.db"] `shouldBe` Right (BrowseZeit "wissen" 2 "custom.db")
      parseArgs ["fetch", "https://www.zeit.de/wissen/2026-05/beispiel"] `shouldBe` Right (FetchArticle "https://www.zeit.de/wissen/2026-05/beispiel" defaultDbPath)
      parseArgs ["batch-fetch", "urls.txt", "custom.db", "500", "2000"] `shouldBe` Right (BatchFetch "urls.txt" "custom.db" (WordFilter (Just 500) (Just 2000)))
      parseArgs ["library", "custom.db"] `shouldBe` Right (ShowLibrary "custom.db")
      parseArgs ["stats"] `shouldBe` Right (ShowStats defaultDbPath)
      parseArgs ["delete-article", "42", "custom.db"] `shouldBe` Right (DeleteArticle 42 "custom.db")
      parseArgs ["ignore-article", "42"] `shouldBe` Right (IgnoreArticle 42 defaultDbPath)
      parseArgs ["unignore-article", "42", "custom.db"] `shouldBe` Right (UnignoreArticle 42 "custom.db")
      parseArgs ["known-sync", "custom.db"] `shouldBe` Right (SyncKnownWords "custom.db")
      parseArgs ["known-import", "words.txt", "custom.db"] `shouldBe` Right (ImportKnownWords "words.txt" "custom.db")
      parseArgs ["known-info"] `shouldBe` Right (KnownWordsInfo defaultDbPath)
      parseArgs ["known-compute", "custom.db"] `shouldBe` Right (ComputeKnownPct "custom.db")
      parseArgs ["lingq-upload", "42", "custom.db"] `shouldBe` Right (UploadLingq 42 "custom.db" defaultSettingsPath)
      parseArgs ["lingq-upload", "42", "custom.db", "settings.dev.json"] `shouldBe` Right (UploadLingq 42 "custom.db" "settings.dev.json")
      parseArgs ["audio-download", "42", "audio-cache", "custom.db"] `shouldBe` Right (DownloadAudio 42 "audio-cache" "custom.db")
      parseArgs ["ignore-url", "https://example.com", "custom.db"] `shouldBe` Right (IgnoreUrl "https://example.com" "custom.db")
      parseArgs ["unignore-url", "https://example.com"] `shouldBe` Right (UnignoreUrl "https://example.com" defaultDbPath)
      parseArgs ["ignored"] `shouldBe` Right (ListIgnored defaultDbPath)
      parseArgs ["settings"] `shouldBe` Right (ShowSettings defaultSettingsPath)
      parseArgs ["settings", "settings.dev.json"] `shouldBe` Right (ShowSettings "settings.dev.json")
      parseArgs ["settings", "set-view", "library"] `shouldBe` Right (SetSettingsView LibraryView defaultSettingsPath)
      parseArgs ["settings", "set-browse-section", "wissen", "settings.dev.json"] `shouldBe` Right (SetSettingsBrowseSection "wissen" "settings.dev.json")
      parseArgs ["settings", "set-date-prefix", "off", "settings.dev.json"] `shouldBe` Right (SetSettingsDatePrefix False "settings.dev.json")
      parseArgs ["settings", "set-collection", "Wissen", "course-1"] `shouldBe` Right (SetSettingsSectionCollection "Wissen" "course-1" defaultSettingsPath)
      parseArgs ["settings", "clear-collection", "Wissen"] `shouldBe` Right (ClearSettingsSectionCollection "Wissen" defaultSettingsPath)

    it "rejects invalid settings values" $ do
      parseArgs ["settings", "set-view", "nope"] `shouldSatisfy` isLeft
      parseArgs ["settings", "set-date-prefix", "maybe"] `shouldSatisfy` isLeft

  describe "Browse use case" $ do
    it "hides browse summaries whose URLs were ignored before fetching" $ do
      let visible =
            ArticleSummary
              { summaryId = Nothing
              , summaryUrl = "https://example.com/visible"
              , summaryTitle = "Visible"
              , summarySection = "Wissen"
              , summaryWordCount = 0
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ignored = visible {summaryUrl = "https://example.com/ignored", summaryTitle = "Ignored"}
      hideIgnoredSummaries (Set.singleton "https://example.com/ignored") [visible, ignored]
        `shouldBe` [visible]

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
    it "derives upload config from persisted preferences" $ do
      let day = fromGregorian 2026 5 2
          config =
            uploadConfigFromPreferences
              day
              (Just "fallback-course")
              False
              (Map.fromList [("Wissen", "wissen-course")])
      config
        `shouldBe` BatchUploadConfig
          { uploadLanguageCode = "de"
          , uploadFallbackCollection = Just "fallback-course"
          , uploadSectionCollections = Map.fromList [("Wissen", "wissen-course")]
          , uploadDatePrefixEnabled = False
          , uploadDay = day
          }

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

    it "deletes and hides articles by id" $ do
      withLibrary ":memory:" $ \db -> do
        firstId <- saveArticleSqlite db demoArticle
        secondId <- saveArticleSqlite db demoArticle {articleUrl = "https://example.com/2", articleTitle = "Other"}

        setIgnoredSqlite db firstId True
        visibleAfterIgnore <- getArticlesSqlite db (WordFilter Nothing Nothing)
        map summaryId visibleAfterIgnore `shouldMatchList` [Just secondId]

        setIgnoredSqlite db firstId False
        visibleAfterUnignore <- getArticlesSqlite db (WordFilter Nothing Nothing)
        map summaryId visibleAfterUnignore `shouldMatchList` [Just firstId, Just secondId]

        deleteArticleSqlite db secondId
        getArticleSqlite db secondId `shouldReturn` Nothing

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

    it "tracks ignored browse URLs before articles are fetched" $ do
      withLibrary ":memory:" $ \db -> do
        ignoreUrlSqlite db "https://www.zeit.de/wissen/2026-05/a"
        ignoreUrlSqlite db "https://www.zeit.de/wissen/2026-05/b"
        getIgnoredUrlsSqlite db `shouldReturn` ["https://www.zeit.de/wissen/2026-05/a", "https://www.zeit.de/wissen/2026-05/b"]

        unignoreUrlSqlite db "https://www.zeit.de/wissen/2026-05/a"
        getIgnoredUrlsSqlite db `shouldReturn` ["https://www.zeit.de/wissen/2026-05/b"]

  describe "JSON settings adapter" $ do
    it "returns defaults when the settings file does not exist" $ do
      withTempSettingsPath $ \path -> do
        loadSettings path `shouldReturn` defaultSettings

    it "persists the settings port values independently" $ do
      withTempSettingsPath $ \path -> do
        let port = jsonSettingsPort path
        saveCurrentView port LingqView
        saveBrowseSection port "wissen"
        saveDatePrefixEnabled port False
        saveSectionCollections port (Map.fromList [("Wissen", "course-1")])

        loadCurrentView port `shouldReturn` LingqView
        loadBrowseSection port `shouldReturn` "wissen"
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

testPorts :: ArticleSummary -> AppPorts Identity
testPorts summary =
  AppPorts
    { zeitPort =
        ZeitPort
          { fetchSections = Identity []
          , fetchArticleList = \sectionId _ -> Identity [summary {summarySection = sectionId}]
          , fetchArticleContent = \_ -> Identity demoArticle
          , loginToZeit = Identity (AuthStatus True Nothing)
          , logoutFromZeit = Identity ()
          }
    , lingqPort =
        LingqPort
          { loginToLingq = \_ _ -> Identity (AuthStatus True Nothing)
          , logoutFromLingq = Identity ()
          , uploadLessonToLingq = \_ _ _ -> Identity (LingqLesson "lesson" "https://lingq.example/lesson")
          , fetchKnownWords = \_ -> Identity []
          }
    , libraryPort =
        LibraryPort
          { loadLibrary = \_ -> Identity [summary]
          , loadArticle = \_ -> Identity (Just demoArticle)
          , saveArticle = \_ -> Identity (ArticleId 1)
          , deleteArticle = \_ -> Identity ()
          , loadStats =
              Identity
                ( LibraryStats
                    { totalArticles = 1
                    , uploadedArticles = 0
                    , averageWordCount = summaryWordCount summary
                    , sectionCounts = Map.singleton (summarySection summary) 1
                    }
                )
          }
    , settingsPort =
        SettingsPort
          { loadCurrentView = Identity BrowseView
          , saveCurrentView = \_ -> Identity ()
          , loadBrowseSection = Identity "index"
          , saveBrowseSection = \_ -> Identity ()
          , loadDatePrefixEnabled = Identity True
          , saveDatePrefixEnabled = \_ -> Identity ()
          , loadSectionCollections = Identity Map.empty
          , saveSectionCollections = \_ -> Identity ()
          }
    }
