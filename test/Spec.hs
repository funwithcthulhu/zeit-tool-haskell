{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Time (UTCTime(..), fromGregorian)
import Data.Map.Strict qualified as Map
import Control.Monad (when)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Functor.Identity (Identity(..), runIdentity)
import Database.SQLite.Simple qualified as SQL
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
import ZeitLingq.Ports (AppPorts(..), AudioPort(..), LibraryPort(..), LingqPort(..), SettingsPort(..), ZeitPort(..))
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
      closedCommands `shouldBe` [PersistCurrentView LibraryView, RefreshLibraryPage defaultLibraryQuery, LoadLibraryStats]

    it "hydrates startup model from the settings port" $ do
      let port =
            SettingsPort
              { loadCurrentView = Identity LingqView
              , saveCurrentView = \_ -> Identity ()
              , loadZeitCookie = Identity "cookie=value"
              , saveZeitCookie = \_ -> Identity ()
              , loadZeitUserAgent = Identity "Browser UA"
              , saveZeitUserAgent = \_ -> Identity ()
              , loadLingqApiKey = Identity "lingq-key"
              , saveLingqApiKey = \_ -> Identity ()
              , loadLingqLanguage = Identity "es"
              , saveLingqLanguage = \_ -> Identity ()
              , loadBrowseSection = Identity "wissen"
              , saveBrowseSection = \_ -> Identity ()
              , loadBrowseFilter = Identity (WordFilter (Just 300) (Just 2000))
              , saveBrowseFilter = \_ -> Identity ()
              , loadBrowseOnlyNew = Identity False
              , saveBrowseOnlyNew = \_ -> Identity ()
              , loadLingqFilter = Identity (WordFilter (Just 500) Nothing)
              , saveLingqFilter = \_ -> Identity ()
              , loadLingqOnlyNotUploaded = Identity False
              , saveLingqOnlyNotUploaded = \_ -> Identity ()
              , loadDatePrefixEnabled = Identity False
              , saveDatePrefixEnabled = \_ -> Identity ()
              , loadLingqFallbackCollection = Identity (Just "fallback-course")
              , saveLingqFallbackCollection = \_ -> Identity ()
              , loadSectionCollections = Identity (Map.fromList [("Wissen", "course-1")])
              , saveSectionCollections = \_ -> Identity ()
              , loadRowDensity = Identity ComfortableRows
              , saveRowDensity = \_ -> Identity ()
              , loadUiTheme = Identity LightUiTheme
              , saveUiTheme = \_ -> Identity ()
              }
          model = runIdentity (loadInitialModel port)
      currentView model `shouldBe` LingqView
      zeitCookieText model `shouldBe` "cookie=value"
      zeitUserAgentText model `shouldBe` "Browser UA"
      lingqApiKeyText model `shouldBe` "lingq-key"
      lingqLanguage model `shouldBe` "es"
      browseSectionId model `shouldBe` "wissen"
      browsePage model `shouldBe` 1
      browseFilter model `shouldBe` WordFilter (Just 300) (Just 2000)
      browseOnlyNew model `shouldBe` False
      lingqFilter model `shouldBe` WordFilter (Just 500) Nothing
      rowDensity model `shouldBe` ComfortableRows
      uiTheme model `shouldBe` LightUiTheme
      lingqOnlyNotUploaded model `shouldBe` False
      datePrefixEnabled model `shouldBe` False
      lingqFallbackCollection model `shouldBe` Just "fallback-course"
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
          browseModel = initialModel {browseSectionId = "wissen", browsePage = 2}
      snd (update (BrowseSectionSelected "kultur") initialModel)
        `shouldBe` [PersistBrowseSection "kultur", RefreshBrowse "kultur" 1 False]
      browsePage (fst (update (BrowseSectionSelected "kultur") browseModel)) `shouldBe` 1
      snd (update (BrowsePageChanged 3) browseModel)
        `shouldBe` [RefreshBrowse "wissen" 3 False]
      snd (update (BrowseFilterChanged filters) browseModel)
        `shouldBe` [PersistBrowseFilter filters]
      snd (update (LibraryFilterChanged filters) initialModel)
        `shouldBe` [RefreshLibraryPage defaultLibraryQuery {libraryWordFilter = filters}]
      snd (update (LingqFilterChanged filters) initialModel)
        `shouldBe` [PersistLingqFilter filters, RefreshLingqLibrary filters True]

    it "updates rich library queries for search, toggles and paging" $ do
      let baseQuery = defaultLibraryQuery {libraryOffset = 30}
          baseModel = initialModel {libraryQuery = baseQuery}
          (searchModel, searchCommands) = update (LibrarySearchChanged " Alpha ") baseModel
          (sectionModel, sectionCommands) = update (LibrarySectionChanged "Wissen") baseModel
          (ignoredModel, ignoredCommands) = update (LibraryIncludeIgnoredChanged True) baseModel
          (onlyIgnoredModel, onlyIgnoredCommands) = update (LibraryOnlyIgnoredChanged True) baseModel
          (onlyNotUploadedModel, onlyNotUploadedCommands) = update (LibraryOnlyNotUploadedChanged True) baseModel
          (sortModel, sortCommands) = update (LibrarySortChanged LibrarySortLongest) baseModel
          (presetModel, presetCommands) = update (LibraryPresetChanged LibraryPresetStandardReads) baseModel
          (duplicatePresetModel, duplicatePresetCommands) = update (LibraryPresetChanged LibraryPresetDuplicateReview) baseModel
          (groupModel, groupCommands) = update (LibraryGroupBySectionChanged True) baseModel
          (collapsedModel, collapsedCommands) = update (LibrarySectionCollapseToggled "Wissen") initialModel
          (expandedModel, expandedCommands) = update (LibrarySectionCollapseToggled "Wissen") collapsedModel
          (pageModel, pageCommands) = update (LibraryPageChanged 60) baseModel
      libraryQuery searchModel `shouldBe` baseQuery {librarySearch = Just "Alpha", libraryOffset = 0}
      libraryPreset searchModel `shouldBe` LibraryPresetCustom
      searchCommands `shouldBe` [RefreshLibraryPage (libraryQuery searchModel)]
      libraryQuery sectionModel `shouldBe` baseQuery {librarySection = Just "Wissen", libraryOffset = 0}
      sectionCommands `shouldBe` [RefreshLibraryPage (libraryQuery sectionModel)]
      libraryQuery ignoredModel `shouldBe` baseQuery {libraryIncludeIgnored = True, libraryOnlyIgnored = False, libraryOffset = 0}
      ignoredCommands `shouldBe` [RefreshLibraryPage (libraryQuery ignoredModel)]
      libraryQuery onlyIgnoredModel `shouldBe` baseQuery {libraryIncludeIgnored = True, libraryOnlyIgnored = True, libraryOffset = 0}
      onlyIgnoredCommands `shouldBe` [RefreshLibraryPage (libraryQuery onlyIgnoredModel)]
      libraryQuery onlyNotUploadedModel `shouldBe` baseQuery {libraryOnlyNotUploaded = True, libraryOffset = 0}
      onlyNotUploadedCommands `shouldBe` [RefreshLibraryPage (libraryQuery onlyNotUploadedModel)]
      libraryQuery sortModel `shouldBe` baseQuery {librarySort = LibrarySortLongest, libraryOffset = 0}
      sortCommands `shouldBe` [RefreshLibraryPage (libraryQuery sortModel)]
      libraryPreset presetModel `shouldBe` LibraryPresetStandardReads
      libraryQuery presetModel
        `shouldBe` defaultLibraryQuery
          { libraryWordFilter = WordFilter (Just 900) (Just 1800)
          , libraryOnlyNotUploaded = True
          }
      presetCommands `shouldBe` [RefreshLibraryPage (libraryQuery presetModel)]
      libraryQuery duplicatePresetModel
        `shouldBe` defaultLibraryQuery
          { libraryIncludeIgnored = True
          , libraryOnlyDuplicateTitles = True
          , librarySort = LibrarySortTitle
          }
      duplicatePresetCommands `shouldBe` [RefreshLibraryPage (libraryQuery duplicatePresetModel)]
      libraryGroupBySection groupModel `shouldBe` True
      libraryQuery groupModel `shouldBe` baseQuery {libraryLimit = 5000, libraryOffset = 0}
      groupCommands `shouldBe` [RefreshLibraryPage (libraryQuery groupModel)]
      libraryCollapsedSections collapsedModel `shouldBe` Set.singleton "Wissen"
      collapsedCommands `shouldBe` []
      libraryCollapsedSections expandedModel `shouldBe` Set.empty
      expandedCommands `shouldBe` []
      libraryQuery pageModel `shouldBe` baseQuery {libraryOffset = 60}
      pageCommands `shouldBe` [RefreshLibraryPage (libraryQuery pageModel)]

    it "turns GUI row actions into app commands" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 15)
              , summaryUrl = "https://example.com/fetch"
              , summaryTitle = "Fetchable"
              , summarySection = "Wissen"
              , summaryWordCount = 321
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
          }
      snd (update (ZeitCookieLoginRequested "cookie=value") initialModel)
        `shouldBe` [LoginZeitWithCookie "cookie=value"]
      snd (update (ZeitBrowserSessionLoginRequested "cookie=value" "Browser UA") initialModel)
        `shouldBe` [LoginZeitWithBrowserSession "cookie=value" "Browser UA"]
      snd (update ZeitLogoutRequested initialModel)
        `shouldBe` [LogoutZeit]
      snd (update (LingqApiKeyLoginRequested "api-key") initialModel)
        `shouldBe` [LoginLingqWithApiKey "api-key"]
      snd (update (LingqPasswordLoginRequested "user" "pass") initialModel)
        `shouldBe` [LoginLingqWithPassword "user" "pass"]
      snd (update LingqLogoutRequested initialModel)
        `shouldBe` [LogoutLingq]
      snd (update (BrowseArticleFetchRequested summary) initialModel)
        `shouldBe` [FetchAndSaveArticle summary]
      snd (update (BrowseArticlePreviewRequested summary) initialModel)
        `shouldBe` [PersistCurrentView ArticleView, PreviewArticle "https://example.com/fetch"]
      snd (update (ArticleDeleteRequested (ArticleId 15)) initialModel)
        `shouldBe` [DeleteSavedArticle (ArticleId 15)]
      snd (update (ArticleIgnoredChanged (ArticleId 15) True) initialModel)
        `shouldBe` [SetArticleIgnored (ArticleId 15) True]
      snd (update (ArticleUploadRequested (fromGregorian 2026 5 2) (Just "fallback") (ArticleId 15)) initialModel)
        `shouldBe` [UploadSavedArticle (fromGregorian 2026 5 2) "de" (Just "fallback") Map.empty True (ArticleId 15)]
      snd (update (BrowseArticleHidden "https://example.com/fetch") initialModel)
        `shouldBe` [SetBrowseUrlIgnored "https://example.com/fetch"]
      snd (update (BrowseBatchFetchRequested [summary]) initialModel)
        `shouldBe` [FetchAndSaveArticles (WordFilter Nothing Nothing) [summary]]
      snd (update (LingqBatchUploadRequested (fromGregorian 2026 5 2) Nothing [summary]) initialModel)
        `shouldBe` [UploadSavedArticles (fromGregorian 2026 5 2) "de" Nothing Map.empty True [ArticleId 15]]
      snd (update (LingqStatusSyncRequested "de" "12") initialModel)
        `shouldBe` [SyncLingqStatus "de" "12"]
      lingqSelectedIds (fst (update (LingqSelectionToggled (ArticleId 15)) initialModel))
        `shouldBe` Set.singleton (ArticleId 15)
      lingqSelectedIds (fst (update (LingqSelectionChanged (Set.singleton (ArticleId 15))) initialModel))
        `shouldBe` Set.singleton (ArticleId 15)
      snd (update (ArticleAudioDownloadRequested "audio" (ArticleId 15)) initialModel)
        `shouldBe` [DownloadArticleAudio "audio" (ArticleId 15)]
      snd (update (ArticleAudioOpenRequested (ArticleId 15)) initialModel)
        `shouldBe` [OpenArticleAudio (ArticleId 15)]
      snd (update (KnownWordsSyncRequested "de") initialModel)
        `shouldBe` [SyncKnownWordsFromLingq "de"]
      snd (update (KnownWordsImportRequested "de" "eins" False) initialModel)
        `shouldBe` [ImportKnownWordText "de" "eins" False]
      snd (update (KnownWordsComputeRequested "de") initialModel)
        `shouldBe` [ComputeKnownPercentagesFor "de"]
      snd (update (KnownWordsClearRequested "de") initialModel)
        `shouldBe` [ClearKnownWords "de"]
      snd (update (LingqCollectionsRefreshRequested "de") initialModel)
        `shouldBe` [RefreshLingqCollections "de"]
      snd (update LingqLanguagesRefreshRequested initialModel)
        `shouldBe` [RefreshLingqLanguages]
      let (languageModel, languageCommands) = update (LingqLanguageChanged "es") initialModel
      lingqLanguage languageModel `shouldBe` "es"
      languageCommands
        `shouldBe` [ PersistLingqLanguage "es"
                   , LoadKnownWordsInfo "es"
                   , PersistLingqFallbackCollection Nothing
                   , PersistSectionCollections Map.empty
                   ]
      snd (update (LingqFallbackCollectionChanged "course-1") initialModel)
        `shouldBe` [PersistLingqFallbackCollection (Just "course-1")]
      snd (update LibraryDeleteIgnoredRequested initialModel)
        `shouldBe` [DeleteIgnoredArticles]
      snd (update (LibraryDeleteOlderRequested (dayTime 3) True False) initialModel)
        `shouldBe` [DeleteOlderArticles (dayTime 3) True False]

    it "manages queued and completed batch jobs purely" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 21)
              , summaryUrl = "https://example.com/queue"
              , summaryTitle = "Queued"
              , summarySection = "Wissen"
              , summaryWordCount = 500
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          (queuedModel, queuedCommands) = update (FetchJobQueued "Fetch queue" [summary]) initialModel
      queuedCommands `shouldBe` []
      nextJobId queuedModel `shouldBe` 2
      length (queuedJobs queuedModel) `shouldBe` 1
      queuedJobs queuedModel
        `shouldBe` [QueuedFetchJob 1 "Fetch queue" (browseFilter initialModel) [summary]]

      let (pausedModel, _) = update (JobQueuePausedChanged True) queuedModel
      jobQueuePaused pausedModel `shouldBe` True

      let job = head (queuedJobs queuedModel)
          (startedModel, _) = update (QueuedJobStarted job) queuedModel
      queuedJobs startedModel `shouldBe` []

      let completed = CompletedJob 1 FetchJob "Fetch queue" "saved 1" True
          (completedModel, _) = update (CompletedJobRecorded completed) startedModel
      completedJobs completedModel `shouldBe` [completed]
      queuedJobs (fst (update QueuedJobsCleared queuedModel)) `shouldBe` []
      completedJobs (fst (update CompletedJobsCleared completedModel)) `shouldBe` []

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
      runIdentity (Runtime.runCommand ports (RefreshBrowse "kultur" 2 False))
        `shouldBe` [BrowseArticlesLoaded [summary {summarySection = "kultur"}]]
      runIdentity (Runtime.runCommand ports (RefreshLibrary filters))
        `shouldBe` [LibraryArticlesLoaded [summary]]
      runIdentity (Runtime.runCommand ports (RefreshLibraryPage defaultLibraryQuery))
        `shouldBe` [LibraryPageLoaded (LibraryPage [summary] 1)]
      runIdentity (Runtime.runCommand ports LoadLibraryStats)
        `shouldBe` [ LibraryStatsLoaded
                       ( LibraryStats
                           { totalArticles = 1
                           , uploadedArticles = 0
                           , averageWordCount = summaryWordCount summary
                           , sectionCounts = Map.singleton (summarySection summary) 1
                           }
                       )
                   ]
      runIdentity (Runtime.runCommand ports (RefreshLingqLibrary filters True))
        `shouldBe` [LingqArticlesLoaded [summary]]
      runIdentity (Runtime.runCommand ports (LoadArticle (ArticleId 1)))
        `shouldBe` [ArticleContentLoaded demoArticle]
      runIdentity (Runtime.runCommand ports (PreviewArticle "https://example.com/runtime"))
        `shouldBe` [ArticleContentLoaded demoArticle]

    it "connects and disconnects account settings through runtime ports" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 12)
              , summaryUrl = "https://example.com/auth"
              , summaryTitle = "Auth"
              , summarySection = "Wissen"
              , summaryWordCount = 555
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ports = testPorts summary
      runIdentity (Runtime.runCommand ports (LoginZeitWithCookie "cookie=value"))
        `shouldBe` [ZeitStatusChanged (AuthStatus True (Just "cookie session")), Notify SuccessNotice "Saved Zeit cookie session."]
      runIdentity (Runtime.runCommand ports (LoginZeitWithBrowserSession "cookie=value" "Browser UA"))
        `shouldBe` [ ZeitStatusChanged (AuthStatus True (Just "browser session"))
                   , ZeitCookieChanged "cookie=value"
                   , ZeitUserAgentChanged "Browser UA"
                   , Notify SuccessNotice "Imported Zeit browser session."
                   ]
      runIdentity (Runtime.runCommand ports LogoutZeit)
        `shouldBe` [ZeitStatusChanged (AuthStatus False (Just "disconnected")), ZeitCookieChanged "", Notify SuccessNotice "Disconnected Zeit session."]
      runIdentity (Runtime.runCommand ports (LoginLingqWithApiKey "api-key"))
        `shouldBe` [LingqStatusChanged (AuthStatus True (Just "API key")), Notify SuccessNotice "Connected LingQ API key.", RefreshCurrentView]
      runIdentity (Runtime.runCommand ports LogoutLingq)
        `shouldBe` [ LingqStatusChanged (AuthStatus False (Just "disconnected"))
                   , LingqApiKeyChanged ""
                   , LingqPasswordChanged ""
                   , Notify SuccessNotice "Disconnected LingQ."
                   ]

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

    it "fetches, saves, and deletes articles through runtime ports" $ do
      let summary =
            ArticleSummary
              { summaryId = Nothing
              , summaryUrl = "https://example.com/save"
              , summaryTitle = "Save Me"
              , summarySection = "Wissen"
              , summaryWordCount = 0
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ports = testPorts summary
      runIdentity (Runtime.runCommand ports (FetchAndSaveArticle summary))
        `shouldBe` [ Notify SuccessNotice "Saved article Save Me."
                   , LibraryArticlesLoaded
                       [ summary
                           { summaryId = Just (ArticleId 1)
                           , summarySection = "Wissen"
                           , summaryWordCount = 4
                           }
                       ]
                   , RefreshCurrentView
                   ]
      runIdentity (Runtime.runCommand ports (DeleteSavedArticle (ArticleId 1)))
        `shouldBe` [Notify SuccessNotice "Deleted article.", ArticleClosed]
      runIdentity (Runtime.runCommand ports (SetArticleIgnored (ArticleId 1) True))
        `shouldBe` [Notify SuccessNotice "Article ignored.", RefreshCurrentView]

    it "uploads saved articles and marks them uploaded" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/upload"
              , summaryTitle = "Upload Me"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports =
            basePorts
              { libraryPort =
                  (libraryPort basePorts)
                    { loadArticle = \_ -> Identity (Just (demoArticle {articleId = Just (ArticleId 1)}))
                    }
              }
      runIdentity (Runtime.runCommand ports (UploadSavedArticle (fromGregorian 2026 5 2) "de" Nothing Map.empty True (ArticleId 1)))
        `shouldBe` [ Notify SuccessNotice "Uploaded 2026-05-02 - Demo to LingQ."
                   , RefreshCurrentView
                   ]

    it "batch-uploads saved articles through the runtime" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/upload-batch"
              , summaryTitle = "Upload Batch"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports =
            basePorts
              { libraryPort =
                  (libraryPort basePorts)
                    { loadArticle = \_ -> Identity (Just (demoArticle {articleId = Just (ArticleId 1)}))
                    }
              }
      runIdentity (Runtime.runCommand ports (UploadSavedArticles (fromGregorian 2026 5 2) "de" Nothing Map.empty True [ArticleId 1, ArticleId 2]))
        `shouldBe` [ BatchUploadFinished []
                   , Notify SuccessNotice "Batch upload: uploaded 2, failed 0."
                   , RefreshCurrentView
                   ]

    it "filters ignored browse URLs and can hide browse rows" $ do
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
          hidden = visible {summaryUrl = "https://example.com/hidden", summaryTitle = "Hidden"}
          basePorts = testPorts visible
          ports =
            basePorts
              { zeitPort =
                  (zeitPort basePorts)
                    { fetchArticleList = \_ _ -> Identity [visible, hidden]
                    }
              , libraryPort =
                  (libraryPort basePorts)
                    { loadIgnoredUrls = Identity ["https://example.com/hidden"]
                    }
              }
      runIdentity (Runtime.runCommand ports (RefreshBrowse "wissen" 1 False))
        `shouldBe` [BrowseArticlesLoaded [visible]]
      runIdentity (Runtime.runCommand ports (RefreshBrowse "wissen" 1 True))
        `shouldBe` [BrowseArticlesLoaded [visible, hidden {summaryIgnored = True}]]
      runIdentity (Runtime.runCommand ports (SetBrowseUrlIgnored "https://example.com/new"))
        `shouldBe` [Notify SuccessNotice "Article hidden from browse.", RefreshCurrentView]
      runIdentity (Runtime.runCommand ports (SetBrowseUrlUnignored "https://example.com/hidden"))
        `shouldBe` [Notify SuccessNotice "Article unhidden from browse.", RefreshCurrentView]

    it "batch-fetches visible browse rows through the runtime" $ do
      let first =
            ArticleSummary
              { summaryId = Nothing
              , summaryUrl = "https://example.com/one"
              , summaryTitle = "One"
              , summarySection = "Wissen"
              , summaryWordCount = 0
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          second = first {summaryUrl = "https://example.com/two", summaryTitle = "Two"}
          ports = testPorts first
      runIdentity (Runtime.runCommand ports (FetchAndSaveArticles (WordFilter Nothing Nothing) [first, second]))
        `shouldBe` [ BatchFetchFinished []
                   , Notify SuccessNotice "Batch fetch: saved 2, skipped 0, failed 0."
                   , RefreshCurrentView
                   ]

    it "downloads article audio and reloads the article" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/audio"
              , summaryTitle = "Audio"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports =
            basePorts
              { audioPort =
                  AudioPort
                    { downloadArticleAudioFile = \audioDir _ -> Identity (audioDir <> "\\demo.mp3")
                    , openAudioFile = \_ -> Identity ()
                    }
              , libraryPort =
                  (libraryPort basePorts)
                    { loadArticle = \_ -> Identity (Just (demoArticle {articleId = Just (ArticleId 1), articleAudioUrl = Just "https://cdn.example/demo.mp3"}))
                    }
              }
      runIdentity (Runtime.runCommand ports (DownloadArticleAudio "audio" (ArticleId 1)))
        `shouldBe` [ Notify SuccessNotice "Saved audio: audio\\demo.mp3"
                   , RefreshCurrentView
                   ]
      let openPorts =
            ports
              { libraryPort =
                  (libraryPort ports)
                    { loadArticle = \_ -> Identity (Just (demoArticle {articleId = Just (ArticleId 1), articleAudioPath = Just "audio\\demo.mp3"}))
                    }
              }
      runIdentity (Runtime.runCommand openPorts (OpenArticleAudio (ArticleId 1)))
        `shouldBe` [Notify SuccessNotice "Opened audio file."]

    it "syncs known words and refreshes cached percentages" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/known"
              , summaryTitle = "Known"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports =
            basePorts
              { lingqPort =
                  (lingqPort basePorts)
                    { fetchKnownWords = \_ -> Identity ["eins", "drei"]
                    }
              , libraryPort =
                  (libraryPort basePorts)
                    { replaceKnownWords = \_ stems -> Identity (Set.size stems)
                    , computeKnownPercentages = \_ -> Identity (Right 1)
                    }
              }
      runIdentity (Runtime.runCommand ports (SyncKnownWordsFromLingq "de"))
        `shouldBe` [ Notify SuccessNotice "Synced 2 known stems and updated 1 articles."
                   , KnownWordsInfoLoaded 2
                   , RefreshCurrentView
                   ]
      runIdentity (Runtime.runCommand ports (ImportKnownWordText "de" "eins\ndrei" False))
        `shouldBe` [ Notify SuccessNotice "Imported 2 stems (0 total) and updated 1 articles."
                   , KnownWordsInfoLoaded 0
                   , KnownWordsImportTextChanged ""
                   , RefreshCurrentView
                   ]
      runIdentity (Runtime.runCommand ports (ComputeKnownPercentagesFor "de"))
        `shouldBe` [Notify SuccessNotice "Updated known-word estimates for 1 articles.", RefreshCurrentView]
      runIdentity (Runtime.runCommand ports (LoadKnownWordsInfo "de"))
        `shouldBe` [KnownWordsInfoLoaded 0]
      runIdentity (Runtime.runCommand ports (ClearKnownWords "de"))
        `shouldBe` [ Notify SuccessNotice "Cleared known words and cached known-word percentages."
                   , KnownWordsInfoLoaded 0
                   , RefreshCurrentView
                   ]

    it "loads LingQ collections through the runtime" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/collections"
              , summaryTitle = "Collections"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ports =
            (testPorts summary)
              { lingqPort =
                  (lingqPort (testPorts summary))
                    { fetchCollections = \_ -> Identity [LingqCollection "12" "Wissen" 3]
                    }
              }
      runIdentity (Runtime.runCommand ports (RefreshLingqCollections "de"))
        `shouldBe` [LingqCollectionsLoaded [LingqCollection "12" "Wissen" 3]]
      runIdentity (Runtime.runCommand ports RefreshLingqLanguages)
        `shouldBe` [LingqLanguagesLoaded [LingqLanguage "de" "German"]]

    it "syncs local upload status from a LingQ collection" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/original"
              , summaryTitle = "2026-05-02 - Existing Lesson"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          ports =
            (testPorts summary)
              { lingqPort =
                  (lingqPort (testPorts summary))
                    { fetchCollectionLessons =
                        \_ _ ->
                          Identity
                            [ LingqRemoteLesson
                                "99"
                                "Existing Lesson"
                                (Just "https://example.com/original")
                                "https://www.lingq.com/de/learn/lesson/99/"
                            ]
                    }
              }
      runIdentity (Runtime.runCommand ports (SyncLingqStatus "de" "12"))
        `shouldBe` [ Notify SuccessNotice "Synced LingQ status: scanned 1 lesson(s), matched 1 article(s), ambiguous 0."
                   , RefreshCurrentView
                   ]

    it "deletes ignored and old articles through runtime ports" $ do
      let summary =
            ArticleSummary
              { summaryId = Just (ArticleId 1)
              , summaryUrl = "https://example.com/delete-old"
              , summaryTitle = "Delete Old"
              , summarySection = "Wissen"
              , summaryWordCount = 4
              , summaryIgnored = False
              , summaryUploaded = False
              , summaryKnownPct = Nothing
              }
          basePorts = testPorts summary
          ports =
            basePorts
              { libraryPort =
                  (libraryPort basePorts)
                    { deleteIgnoredArticles = Identity 2
                    , deleteOlderArticles = \_ _ _ -> Identity 3
                    }
              }
      runIdentity (Runtime.runCommand ports DeleteIgnoredArticles)
        `shouldBe` [Notify SuccessNotice "Deleted 2 ignored article(s).", RefreshCurrentView]
      runIdentity (Runtime.runCommand ports (DeleteOlderArticles (dayTime 3) True False))
        `shouldBe` [Notify SuccessNotice "Deleted 3 old article(s).", RefreshCurrentView]

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
      map navLabel (vmNavItems viewModel) `shouldBe` ["Browse", "Library", "LingQ", "Zeit", "Diagnostics"]
      map navActive (vmNavItems viewModel) `shouldBe` [False, True, False, False, False]
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
      rowUploadStatus (articleRowView (summary {summaryIgnored = True})) `shouldBe` "ignored"

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
    it "shows help by default and keeps the demo explicit" $ do
      parseArgs [] `shouldBe` Right ShowHelp
      parseArgs ["help"] `shouldBe` Right ShowHelp
      parseArgs ["--help"] `shouldBe` Right ShowHelp
      parseArgs ["demo"] `shouldBe` Right ShowDemo

    it "parses browse, fetch and library commands" $ do
      parseArgs ["browse", "wissen", "2"] `shouldBe` Right (BrowseZeit "wissen" 2 defaultDbPath)
      parseArgs ["browse", "wissen", "2", "custom.db"] `shouldBe` Right (BrowseZeit "wissen" 2 "custom.db")
      parseArgs ["fetch", "https://www.zeit.de/wissen/2026-05/beispiel"] `shouldBe` Right (FetchArticle "https://www.zeit.de/wissen/2026-05/beispiel" defaultDbPath)
      parseArgs ["batch-fetch", "urls.txt", "custom.db", "500", "2000"] `shouldBe` Right (BatchFetch "urls.txt" "custom.db" (WordFilter (Just 500) (Just 2000)))
      parseArgs ["library", "custom.db"] `shouldBe` Right (ShowLibrary "custom.db")
      parseArgs ["stats"] `shouldBe` Right (ShowStats defaultDbPath)
      parseArgs ["delete-article", "42", "custom.db"] `shouldBe` Right (DeleteArticle 42 "custom.db")
      parseArgs ["delete-older-than", "30", "custom.db"] `shouldBe` Right (DeleteOlderThan 30 False False "custom.db")
      parseArgs ["delete-older-than-uploaded", "30"] `shouldBe` Right (DeleteOlderThan 30 True False defaultDbPath)
      parseArgs ["delete-older-than-unuploaded", "30"] `shouldBe` Right (DeleteOlderThan 30 False True defaultDbPath)
      parseArgs ["delete-ignored", "custom.db"] `shouldBe` Right (DeleteIgnored "custom.db")
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
      parseArgs ["settings", "set-view", "diagnostics"] `shouldBe` Right (SetSettingsView DiagnosticsView defaultSettingsPath)
      parseArgs ["settings", "set-browse-section", "wissen", "settings.dev.json"] `shouldBe` Right (SetSettingsBrowseSection "wissen" "settings.dev.json")
      parseArgs ["settings", "set-date-prefix", "off", "settings.dev.json"] `shouldBe` Right (SetSettingsDatePrefix False "settings.dev.json")
      parseArgs ["settings", "set-collection", "Wissen", "course-1"] `shouldBe` Right (SetSettingsSectionCollection "Wissen" "course-1" defaultSettingsPath)
      parseArgs ["settings", "clear-collection", "Wissen"] `shouldBe` Right (ClearSettingsSectionCollection "Wissen" defaultSettingsPath)

    it "parses the simplified grouped commands and named flags" $ do
      parseArgs ["topics"] `shouldBe` Right ListSections
      parseArgs ["browse", "--page", "3", "--db", "custom.db"] `shouldBe` Right (BrowseZeit "index" 3 "custom.db")
      parseArgs ["browse", "wissen", "--page=3", "--db=custom.db"] `shouldBe` Right (BrowseZeit "wissen" 3 "custom.db")
      parseArgs ["read", "https://www.zeit.de/wissen/2026-05/beispiel", "--db", "custom.db"] `shouldBe` Right (FetchArticle "https://www.zeit.de/wissen/2026-05/beispiel" "custom.db")
      parseArgs ["fetch-list", "urls.txt", "--min", "500", "--max", "2000", "--db", "custom.db"] `shouldBe` Right (BatchFetch "urls.txt" "custom.db" (WordFilter (Just 500) (Just 2000)))
      parseArgs ["list", "--db", "custom.db"] `shouldBe` Right (ShowLibrary "custom.db")
      parseArgs ["hide", "article", "42", "--db", "custom.db"] `shouldBe` Right (IgnoreArticle 42 "custom.db")
      parseArgs ["show", "article", "42"] `shouldBe` Right (UnignoreArticle 42 defaultDbPath)
      parseArgs ["hide", "url", "https://example.com", "--db", "custom.db"] `shouldBe` Right (IgnoreUrl "https://example.com" "custom.db")
      parseArgs ["show", "https://example.com"] `shouldBe` Right (UnignoreUrl "https://example.com" defaultDbPath)
      parseArgs ["hidden", "--db", "custom.db"] `shouldBe` Right (ListIgnored "custom.db")
      parseArgs ["delete", "article", "42", "--db", "custom.db"] `shouldBe` Right (DeleteArticle 42 "custom.db")
      parseArgs ["delete", "old", "30", "--uploaded", "--db", "custom.db"] `shouldBe` Right (DeleteOlderThan 30 True False "custom.db")
      parseArgs ["delete", "old", "30", "--unuploaded"] `shouldBe` Right (DeleteOlderThan 30 False True defaultDbPath)
      parseArgs ["delete", "ignored", "--db", "custom.db"] `shouldBe` Right (DeleteIgnored "custom.db")
      parseArgs ["known", "sync", "--db", "custom.db"] `shouldBe` Right (SyncKnownWords "custom.db")
      parseArgs ["known", "import", "words.txt", "--db", "custom.db"] `shouldBe` Right (ImportKnownWords "words.txt" "custom.db")
      parseArgs ["known", "recompute"] `shouldBe` Right (ComputeKnownPct defaultDbPath)
      parseArgs ["known"] `shouldBe` Right (KnownWordsInfo defaultDbPath)
      parseArgs ["lingq", "upload", "42", "--db", "custom.db", "--settings", "settings.dev.json"] `shouldBe` Right (UploadLingq 42 "custom.db" "settings.dev.json")
      parseArgs ["audio", "download", "42", "--to", "audio-cache", "--db", "custom.db"] `shouldBe` Right (DownloadAudio 42 "audio-cache" "custom.db")
      parseArgs ["settings", "--settings", "settings.dev.json"] `shouldBe` Right (ShowSettings "settings.dev.json")
      parseArgs ["settings", "view", "library", "--settings", "settings.dev.json"] `shouldBe` Right (SetSettingsView LibraryView "settings.dev.json")
      parseArgs ["settings", "topic", "wissen"] `shouldBe` Right (SetSettingsBrowseSection "wissen" defaultSettingsPath)
      parseArgs ["settings", "date-prefix", "off"] `shouldBe` Right (SetSettingsDatePrefix False defaultSettingsPath)
      parseArgs ["settings", "collection", "Wissen", "course-1", "--settings", "settings.dev.json"] `shouldBe` Right (SetSettingsSectionCollection "Wissen" "course-1" "settings.dev.json")
      parseArgs ["settings", "forget-collection", "Wissen"] `shouldBe` Right (ClearSettingsSectionCollection "Wissen" defaultSettingsPath)

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
              "es"
              (Just "fallback-course")
              False
              (Map.fromList [("Wissen", "wissen-course")])
      config
        `shouldBe` BatchUploadConfig
          { uploadLanguageCode = "es"
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
          updater _ _ _ = do
            expectationFailure "updater should not be called for new lessons"
            pure (Left "unexpected update")
          marker _ _ =
            pure ()
      results <- batchUploadArticles uploader updater marker config [demoArticle {articleId = Just (ArticleId 3)}]
      results
        `shouldBe` [ UploadSucceeded
                       (ArticleId 3)
                       "2026-05-02 - Demo"
                       (LingqLesson "de:wissen-course" "lesson:2026-05-02 - Demo")
                   ]

    it "updates existing LingQ lessons instead of creating duplicates" $ do
      let config =
            BatchUploadConfig
              { uploadLanguageCode = "de"
              , uploadFallbackCollection = Just "fallback"
              , uploadSectionCollections = Map.empty
              , uploadDatePrefixEnabled = False
              , uploadDay = fromGregorian 2026 5 2
              }
          existing = LingqLesson "old-lesson" "https://lingq.example/old"
          uploader _ _ _ = do
            expectationFailure "uploader should not be called for existing lessons"
            pure (Left "unexpected upload")
          updater lang lesson article =
            pure (Right (LingqLesson (lang <> ":" <> lessonId lesson) ("updated:" <> articleTitle article)))
          marker _ _ =
            pure ()
      results <-
        batchUploadArticles
          uploader
          updater
          marker
          config
          [demoArticle {articleId = Just (ArticleId 3), articleUploadedLesson = Just existing}]
      results
        `shouldBe` [ UploadSucceeded
                       (ArticleId 3)
                       "Demo"
                       (LingqLesson "de:old-lesson" "updated:Demo")
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
          updater _ _ _ = pure (Left "update failed")
          marker _ _ = expectationFailure "marker should not be called"
      results <- batchUploadArticles uploader updater marker config [demoArticle {articleId = Just (ArticleId 4)}]
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

    it "queries library pages with search, section, ignored, uploaded, and paging filters" $ do
      withLibrary ":memory:" $ \db -> do
        alphaId <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/alpha"
              , articleTitle = "Alpha Wissen"
              , articleSection = "Wissen"
              , articleFetchedAt = Just (dayTime 1)
              }
        betaId <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/beta"
              , articleTitle = "Beta Kultur"
              , articleSection = "Kultur"
              , articleParagraphs = ["eins zwei drei vier funf sechs"]
              , articleFetchedAt = Just (dayTime 2)
              }
        ignoredId <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/ignored"
              , articleTitle = "Ignored Alpha"
              , articleSection = "Wissen"
              , articleFetchedAt = Just (dayTime 3)
              }
        markUploadedSqlite db betaId (LingqLesson "lesson-2" "https://lingq.example/2")
        setIgnoredSqlite db ignoredId True

        page <-
          getArticlesByQuerySqlite db
            LibraryQuery
              { librarySearch = Just "Alpha"
              , librarySection = Just "Wissen"
              , libraryWordFilter = WordFilter Nothing Nothing
              , libraryIncludeIgnored = True
              , libraryOnlyIgnored = False
              , libraryOnlyNotUploaded = True
              , libraryOnlyDuplicateTitles = False
              , librarySort = LibrarySortNewest
              , libraryLimit = 10
              , libraryOffset = 0
              }
        libraryPageTotal page `shouldBe` 2
        map summaryId (libraryPageArticles page) `shouldBe` [Just ignoredId, Just alphaId]

        ignoredPage <-
          getArticlesByQuerySqlite db
            LibraryQuery
              { librarySearch = Nothing
              , librarySection = Nothing
              , libraryWordFilter = WordFilter Nothing Nothing
              , libraryIncludeIgnored = False
              , libraryOnlyIgnored = True
              , libraryOnlyNotUploaded = False
              , libraryOnlyDuplicateTitles = False
              , librarySort = LibrarySortNewest
              , libraryLimit = 10
              , libraryOffset = 0
              }
        map summaryId (libraryPageArticles ignoredPage) `shouldBe` [Just ignoredId]

        paged <-
          getArticlesByQuerySqlite db
            LibraryQuery
              { librarySearch = Nothing
              , librarySection = Nothing
              , libraryWordFilter = WordFilter Nothing Nothing
              , libraryIncludeIgnored = True
              , libraryOnlyIgnored = False
              , libraryOnlyNotUploaded = False
              , libraryOnlyDuplicateTitles = False
              , librarySort = LibrarySortNewest
              , libraryLimit = 1
              , libraryOffset = 1
              }
        libraryPageTotal paged `shouldBe` 3
        map summaryId (libraryPageArticles paged) `shouldBe` [Just betaId]

        sortedByLength <- getArticlesByQuerySqlite db defaultLibraryQuery {libraryIncludeIgnored = True, librarySort = LibrarySortLongest}
        map summaryId (libraryPageArticles sortedByLength) `shouldBe` [Just betaId, Just ignoredId, Just alphaId]

    it "filters duplicate title review pages" $ do
      withLibrary ":memory:" $ \db -> do
        firstId <- saveArticleSqlite db demoArticle {articleUrl = "https://example.com/a", articleTitle = "Same Title"}
        secondId <- saveArticleSqlite db demoArticle {articleUrl = "https://example.com/b", articleTitle = "same title"}
        _ <- saveArticleSqlite db demoArticle {articleUrl = "https://example.com/c", articleTitle = "Different"}

        duplicatePage <-
          getArticlesByQuerySqlite db
            defaultLibraryQuery
              { libraryIncludeIgnored = True
              , libraryOnlyDuplicateTitles = True
              , librarySort = LibrarySortTitle
              }
        libraryPageTotal duplicatePage `shouldBe` 2
        map summaryId (libraryPageArticles duplicatePage) `shouldBe` [Just secondId, Just firstId]

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

    it "migrates older article tables before saving new records" $ do
      withTempDbPath $ \path -> do
        conn <- SQL.open path
        SQL.execute_ conn "CREATE TABLE articles (id INTEGER PRIMARY KEY AUTOINCREMENT, url TEXT UNIQUE NOT NULL, title TEXT NOT NULL)"
        SQL.close conn

        withLibrary path $ \db -> do
          savedId <- saveArticleSqlite db demoArticle
          loaded <- getArticleSqlite db savedId
          fmap articleTitle loaded `shouldBe` Just "Demo"

    it "bulk-deletes ignored and older articles" $ do
      withLibrary ":memory:" $ \db -> do
        oldUploaded <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/old-uploaded"
              , articleTitle = "Old uploaded"
              , articleFetchedAt = Just (dayTime 1)
              }
        oldUnuploaded <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/old-unuploaded"
              , articleTitle = "Old unuploaded"
              , articleFetchedAt = Just (dayTime 1)
              }
        oldIgnored <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/old-ignored"
              , articleTitle = "Old ignored"
              , articleFetchedAt = Just (dayTime 1)
              }
        recent <-
          saveArticleSqlite db
            demoArticle
              { articleUrl = "https://example.com/recent"
              , articleTitle = "Recent"
              , articleFetchedAt = Just (dayTime 5)
              }
        markUploadedSqlite db oldUploaded (LingqLesson "lesson-old" "https://lingq.example/old")
        setIgnoredSqlite db oldIgnored True

        deleteIgnoredSqlite db `shouldReturn` 1
        getArticleSqlite db oldIgnored `shouldReturn` Nothing

        deleteOlderThanSqlite db (dayTime 3) True False `shouldReturn` 1
        getArticleSqlite db oldUploaded `shouldReturn` Nothing
        fmap (fmap articleId) (getArticleSqlite db oldUnuploaded) `shouldReturn` Just (Just oldUnuploaded)

        deleteOlderThanSqlite db (dayTime 3) False True `shouldReturn` 1
        getArticleSqlite db oldUnuploaded `shouldReturn` Nothing
        fmap (fmap articleId) (getArticleSqlite db recent) `shouldReturn` Just (Just recent)

  describe "JSON settings adapter" $ do
    it "returns defaults when the settings file does not exist" $ do
      withTempSettingsPath $ \path -> do
        loadSettings path `shouldReturn` defaultSettings

    it "persists the settings port values independently" $ do
      withTempSettingsPath $ \path -> do
        let port = jsonSettingsPort path
        saveCurrentView port LingqView
        saveZeitCookie port " cookie=value "
        saveZeitUserAgent port " Browser UA "
        saveLingqApiKey port " api-key "
        saveLingqLanguage port " ES "
        saveBrowseSection port "wissen"
        saveBrowseFilter port (WordFilter (Just 250) (Just 1500))
        saveBrowseOnlyNew port False
        saveLingqFilter port (WordFilter (Just 500) Nothing)
        saveLingqOnlyNotUploaded port False
        saveDatePrefixEnabled port False
        saveLingqFallbackCollection port (Just "fallback")
        saveSectionCollections port (Map.fromList [("Wissen", "course-1")])
        saveRowDensity port ComfortableRows
        saveUiTheme port LightUiTheme

        loadCurrentView port `shouldReturn` LingqView
        loadZeitCookie port `shouldReturn` "cookie=value"
        loadZeitUserAgent port `shouldReturn` "Browser UA"
        loadLingqApiKey port `shouldReturn` "api-key"
        loadLingqLanguage port `shouldReturn` "es"
        loadBrowseSection port `shouldReturn` "wissen"
        loadBrowseFilter port `shouldReturn` WordFilter (Just 250) (Just 1500)
        loadBrowseOnlyNew port `shouldReturn` False
        loadLingqFilter port `shouldReturn` WordFilter (Just 500) Nothing
        loadLingqOnlyNotUploaded port `shouldReturn` False
        loadDatePrefixEnabled port `shouldReturn` False
        loadLingqFallbackCollection port `shouldReturn` Just "fallback"
        loadSectionCollections port `shouldReturn` Map.fromList [("Wissen", "course-1")]
        loadRowDensity port `shouldReturn` ComfortableRows
        loadUiTheme port `shouldReturn` LightUiTheme

  describe "LingQ adapter helpers" $ do
    it "normalizes lesson text without flattening paragraphs" $ do
      normalizeLessonText " eins   zwei\nnoch \n\n  drei\tvier " `shouldBe` "eins zwei noch\n\ndrei vier"

    it "parses collection responses with numeric ids" $ do
      let value = decodeValue "{\"results\":[{\"id\":12,\"title\":\"Wissen\",\"lessons_count\":3}]}"
      parseCollectionsValue value `shouldBe` Right [LingqCollection "12" "Wissen" 3]

    it "parses LingQ language responses" $ do
      let value = decodeValue "{\"results\":[{\"code\":\"DE\",\"title\":\"German\"},{\"code\":\"es\",\"title\":\"Spanish\"}]}"
      parseLanguagesValue value `shouldBe` Right [LingqLanguage "de" "German", LingqLanguage "es" "Spanish"]

    it "parses LingQ collection lesson responses" $ do
      let value = decodeValue "{\"results\":[{\"id\":99,\"title\":\"Existing Lesson\",\"original_url\":\"https://example.com/original\"}]}"
      parseCollectionLessonsValue "de" value
        `shouldBe` Right
          [ LingqRemoteLesson
              "99"
              "Existing Lesson"
              (Just "https://example.com/original")
              "https://www.lingq.com/de/learn/lesson/99/"
          ]

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

    it "extracts article audio from source tags and JSON-LD" $ do
      let sourceHtml =
            "<html><head><title>Audio | ZEIT ONLINE</title></head><body><article><h1>Audio</h1><p>Das ist ein Absatz mit genug Worten fuer den Parser.</p><p>Noch ein Absatz mit sauberem Text und Inhalt.</p><audio><source src=\"https://cdn.example/audio.m4a\"></audio></article></body></html>"
          jsonHtml =
            "<html><head><title>Audio JSON | ZEIT ONLINE</title><script type=\"application/ld+json\">{\"@type\":\"NewsArticle\",\"audio\":{\"@type\":\"AudioObject\",\"contentUrl\":\"https://cdn.example/audio.mp3\"}}</script></head><body><article><h1>Audio JSON</h1><p>Das ist ein Absatz mit genug Worten fuer den Parser.</p><p>Noch ein Absatz mit sauberem Text und Inhalt.</p></article></body></html>"
          relativeHtml =
            "<html><head><title>Audio relative | ZEIT ONLINE</title></head><body><article><h1>Audio relative</h1><p>Das ist ein Absatz mit genug Worten fuer den Parser.</p><p>Noch ein Absatz mit sauberem Text und Inhalt.</p><audio src=\"/audio/demo.mp3\"></audio></article></body></html>"
      fmap articleAudioUrl (extractArticleContent "https://www.zeit.de/wissen/2026-05/audio" sourceHtml)
        `shouldBe` Right (Just "https://cdn.example/audio.m4a")
      fmap articleAudioUrl (extractArticleContent "https://www.zeit.de/wissen/2026-05/audio-json" jsonHtml)
        `shouldBe` Right (Just "https://cdn.example/audio.mp3")
      fmap articleAudioUrl (extractArticleContent "https://www.zeit.de/wissen/2026-05/audio-relative" relativeHtml)
        `shouldBe` Right (Just "https://www.zeit.de/audio/demo.mp3")

    it "discovers additional article page links for paginated stories" $ do
      let html =
            "<article>\
            \<a href=\"/wissen/2026-05/haskell-test\">same article</a>\
            \<a href=\"/wissen/2026-05/haskell-test/2\">Seite 2</a>\
            \<a href=\"/wissen/2026-05/haskell-test/seite-3\">Nächste Seite</a>\
            \<a href=\"/wissen/2026-05/haskell-test-extra/2\">Seite 2</a>\
            \<a href=\"/wissen/2026-05/other/2\">Seite 2</a>\
            \</article>"
      extractAdditionalArticlePageUrls "https://www.zeit.de/wissen/2026-05/haskell-test" html
        `shouldBe`
          [ "https://www.zeit.de/wissen/2026-05/haskell-test/2"
          , "https://www.zeit.de/wissen/2026-05/haskell-test/seite-3"
          ]

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

dayTime :: Int -> UTCTime
dayTime day =
  UTCTime (fromGregorian 2026 5 day) 0

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

withTempDbPath :: (FilePath -> IO a) -> IO a
withTempDbPath action = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "zeit-tool-sqlite-test.db"
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
          , loginToZeitWithCookie = \_ -> Identity (AuthStatus True (Just "cookie session"))
          , loginToZeitWithBrowserSession = \_ _ -> Identity (AuthStatus True (Just "browser session"))
          , logoutFromZeit = Identity ()
          }
    , lingqPort =
        LingqPort
          { loginToLingq = \_ _ -> Identity (AuthStatus True Nothing)
          , loginToLingqWithApiKey = \_ -> Identity (AuthStatus True (Just "API key"))
          , logoutFromLingq = Identity ()
          , uploadLessonToLingq = \_ _ _ -> Identity (LingqLesson "lesson" "https://lingq.example/lesson")
          , updateLessonOnLingq = \_ lesson _ -> Identity lesson
          , fetchLanguages = Identity [LingqLanguage "de" "German"]
          , fetchCollections = \_ -> Identity []
          , fetchCollectionLessons = \_ _ -> Identity []
          , fetchKnownWords = \_ -> Identity []
          }
    , audioPort =
        AudioPort
          { downloadArticleAudioFile = \audioDir _ -> Identity (audioDir <> "\\article.mp3")
          , openAudioFile = \_ -> Identity ()
          }
    , libraryPort =
        LibraryPort
          { loadLibrary = \_ -> Identity [summary]
          , loadLibraryPage = \_ -> Identity (LibraryPage [summary] 1)
          , loadArticle = \_ -> Identity (Just demoArticle)
          , saveArticle = \_ -> Identity (ArticleId 1)
          , deleteArticle = \_ -> Identity ()
          , setArticleIgnored = \_ _ -> Identity ()
          , markArticleUploaded = \_ _ -> Identity ()
          , setArticleAudioPath = \_ _ -> Identity ()
          , loadIgnoredUrls = Identity []
          , ignoreArticleUrl = \_ -> Identity ()
          , unignoreArticleUrl = \_ -> Identity ()
          , deleteIgnoredArticles = Identity 0
          , deleteOlderArticles = \_ _ _ -> Identity 0
          , replaceKnownWords = \_ stems -> Identity (Set.size stems)
          , addKnownWords = \_ stems -> Identity (Set.size stems)
          , clearKnownWords = \_ -> Identity ()
          , clearKnownPercentages = Identity ()
          , computeKnownPercentages = \_ -> Identity (Right 0)
          , knownStemCount = \_ -> Identity 0
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
          , loadZeitCookie = Identity ""
          , saveZeitCookie = \_ -> Identity ()
          , loadZeitUserAgent = Identity defaultZeitUserAgent
          , saveZeitUserAgent = \_ -> Identity ()
          , loadLingqApiKey = Identity ""
          , saveLingqApiKey = \_ -> Identity ()
          , loadLingqLanguage = Identity "de"
          , saveLingqLanguage = \_ -> Identity ()
          , loadBrowseSection = Identity "index"
          , saveBrowseSection = \_ -> Identity ()
          , loadBrowseFilter = Identity (WordFilter Nothing Nothing)
          , saveBrowseFilter = \_ -> Identity ()
          , loadBrowseOnlyNew = Identity True
          , saveBrowseOnlyNew = \_ -> Identity ()
          , loadLingqFilter = Identity (WordFilter Nothing Nothing)
          , saveLingqFilter = \_ -> Identity ()
          , loadLingqOnlyNotUploaded = Identity True
          , saveLingqOnlyNotUploaded = \_ -> Identity ()
          , loadDatePrefixEnabled = Identity True
          , saveDatePrefixEnabled = \_ -> Identity ()
          , loadLingqFallbackCollection = Identity Nothing
          , saveLingqFallbackCollection = \_ -> Identity ()
          , loadSectionCollections = Identity Map.empty
          , saveSectionCollections = \_ -> Identity ()
          , loadRowDensity = Identity CompactRows
          , saveRowDensity = \_ -> Identity ()
          , loadUiTheme = Identity DarkUiTheme
          , saveUiTheme = \_ -> Identity ()
          }
    }
