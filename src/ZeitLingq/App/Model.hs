{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Model
  ( Model(..)
  , emptyAuth
  , initialModel
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import ZeitLingq.Domain.Types

data Model = Model
  { currentView :: View
  , articleReturnView :: View
  , zeitStatus :: AuthStatus
  , lingqStatus :: AuthStatus
  , zeitCookieText :: Text
  , zeitUserAgentText :: Text
  , lingqApiKeyText :: Text
  , lingqUsernameText :: Text
  , lingqPasswordText :: Text
  , lingqLanguage :: Text
  , lingqLanguages :: [LingqLanguage]
  , selectedArticle :: Maybe ArticleSummary
  , selectedArticleContent :: Maybe Article
  , browseArticles :: [ArticleSummary]
  , browseSelectedUrls :: Set Text
  , browseShowHidden :: Bool
  , browseOnlyNew :: Bool
  , browseSearch :: Text
  , libraryArticles :: [ArticleSummary]
  , libraryTotal :: Int
  , libraryStats :: Maybe LibraryStats
  , libraryGroupBySection :: Bool
  , libraryPreset :: LibraryPreset
  , libraryDeleteDaysText :: Text
  , lingqArticles :: [ArticleSummary]
  , lingqSelectedIds :: Set ArticleId
  , lingqCollections :: [LingqCollection]
  , lingqFallbackCollection :: Maybe Text
  , lingqOnlyNotUploaded :: Bool
  , lingqShowKnownImport :: Bool
  , lingqShowSectionMappings :: Bool
  , knownStemTotal :: Int
  , knownImportText :: Text
  , notification :: Maybe Notification
  , activeProgress :: Maybe ProgressStatus
  , queuedJobs :: [QueuedJob]
  , completedJobs :: [CompletedJob]
  , jobQueuePaused :: Bool
  , nextJobId :: Int
  , failedFetches :: [(Text, Text)]
  , failedUploads :: [(ArticleId, Text)]
  , rowDensity :: RowDensity
  , uiTheme :: UiTheme
  , browseSectionId :: Text
  , browsePage :: Int
  , browseFilter :: WordFilter
  , libraryFilter :: WordFilter
  , libraryQuery :: LibraryQuery
  , lingqFilter :: WordFilter
  , datePrefixEnabled :: Bool
  , sectionCollections :: Map Text Text
  } deriving (Eq, Show)

emptyAuth :: AuthStatus
emptyAuth = AuthStatus False Nothing

initialModel :: Model
initialModel =
  Model
    { currentView = BrowseView
    , articleReturnView = LibraryView
    , zeitStatus = emptyAuth
    , lingqStatus = emptyAuth
    , zeitCookieText = ""
    , zeitUserAgentText = ""
    , lingqApiKeyText = ""
    , lingqUsernameText = ""
    , lingqPasswordText = ""
    , lingqLanguage = "de"
    , lingqLanguages = [LingqLanguage "de" "German"]
    , selectedArticle = Nothing
    , selectedArticleContent = Nothing
    , browseArticles = []
    , browseSelectedUrls = Set.empty
    , browseShowHidden = False
    , browseOnlyNew = True
    , browseSearch = ""
    , libraryArticles = []
    , libraryTotal = 0
    , libraryStats = Nothing
    , libraryGroupBySection = False
    , libraryPreset = LibraryPresetAll
    , libraryDeleteDaysText = "30"
    , lingqArticles = []
    , lingqSelectedIds = Set.empty
    , lingqCollections = []
    , lingqFallbackCollection = Nothing
    , lingqOnlyNotUploaded = True
    , lingqShowKnownImport = False
    , lingqShowSectionMappings = False
    , knownStemTotal = 0
    , knownImportText = ""
    , notification = Nothing
    , activeProgress = Nothing
    , queuedJobs = []
    , completedJobs = []
    , jobQueuePaused = False
    , nextJobId = 1
    , failedFetches = []
    , failedUploads = []
    , rowDensity = CompactRows
    , uiTheme = DarkUiTheme
    , browseSectionId = "index"
    , browsePage = 1
    , browseFilter = WordFilter Nothing Nothing
    , libraryFilter = WordFilter Nothing Nothing
    , libraryQuery = defaultLibraryQuery
    , lingqFilter = WordFilter Nothing Nothing
    , datePrefixEnabled = True
    , sectionCollections = Map.empty
    }
