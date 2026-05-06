{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Domain.Types
  ( Article(..)
  , ArticleId(..)
  , ArticleSummary(..)
  , AuthStatus(..)
  , CompletedJob(..)
  , ArticleFetchFailure(..)
  , JobKind(..)
  , LibraryPage(..)
  , LibraryPreset(..)
  , LibraryQuery(..)
  , LibrarySort(..)
  , LibraryStats(..)
  , LingqCollection(..)
  , LingqLanguage(..)
  , LingqLesson(..)
  , LingqRemoteLesson(..)
  , Notification(..)
  , NotificationLevel(..)
  , ProgressStatus(..)
  , QueuedJob(..)
  , RowDensity(..)
  , Section(..)
  , UiTheme(..)
  , ArticleUploadFailure(..)
  , View(..)
  , WordFilter(..)
  , defaultLibraryQuery
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype ArticleId = ArticleId { unArticleId :: Int }
  deriving (Eq, Ord, Show, Generic)

data View
  = BrowseView
  | LibraryView
  | LingqView
  | ZeitLoginView
  | DiagnosticsView
  | ArticleView
  deriving (Eq, Show, Enum, Bounded, Generic)

data NotificationLevel
  = InfoNotice
  | SuccessNotice
  | ErrorNotice
  deriving (Eq, Show, Generic)

data Notification = Notification
  { notificationLevel :: NotificationLevel
  , notificationMessage :: Text
  } deriving (Eq, Show, Generic)

data ProgressStatus = ProgressStatus
  { progressLabel :: Text
  , progressCurrent :: Int
  , progressTotal :: Int
  , progressDetail :: Text
  } deriving (Eq, Show, Generic)

data JobKind
  = FetchJob
  | UploadJob
  deriving (Eq, Show, Enum, Bounded, Generic)

data QueuedJob
  = QueuedFetchJob
      { queuedJobId :: Int
      , queuedJobLabel :: Text
      , queuedFetchFilter :: WordFilter
      , queuedFetchArticles :: [ArticleSummary]
      }
  | QueuedUploadJob
      { queuedJobId :: Int
      , queuedJobLabel :: Text
      , queuedUploadArticles :: [ArticleSummary]
      }
  deriving (Eq, Show, Generic)

data CompletedJob = CompletedJob
  { completedJobId :: Int
  , completedJobKind :: JobKind
  , completedJobLabel :: Text
  , completedJobSummary :: Text
  , completedJobSucceeded :: Bool
  } deriving (Eq, Show, Generic)

data ArticleFetchFailure = ArticleFetchFailure
  { articleFetchFailureUrl :: Text
  , articleFetchFailureReason :: Text
  } deriving (Eq, Show, Generic)

data ArticleUploadFailure = ArticleUploadFailure
  { articleUploadFailureId :: ArticleId
  , articleUploadFailureTitle :: Maybe Text
  , articleUploadFailureReason :: Text
  } deriving (Eq, Show, Generic)

data AuthStatus = AuthStatus
  { authLoggedIn :: Bool
  , authLabel :: Maybe Text
  } deriving (Eq, Show, Generic)

data Section = Section
  { sectionId :: Text
  , sectionLabel :: Text
  , sectionPath :: Text
  } deriving (Eq, Show, Generic)

data WordFilter = WordFilter
  { minWords :: Maybe Int
  , maxWords :: Maybe Int
  } deriving (Eq, Show, Generic)

data LibraryQuery = LibraryQuery
  { librarySearch :: Maybe Text
  , librarySection :: Maybe Text
  , libraryWordFilter :: WordFilter
  , libraryIncludeIgnored :: Bool
  , libraryOnlyIgnored :: Bool
  , libraryOnlyNotUploaded :: Bool
  , libraryOnlyDuplicateTitles :: Bool
  , librarySort :: LibrarySort
  , libraryLimit :: Int
  , libraryOffset :: Int
  } deriving (Eq, Show, Generic)

data LibrarySort
  = LibrarySortNewest
  | LibrarySortOldest
  | LibrarySortLongest
  | LibrarySortShortest
  | LibrarySortTitle
  deriving (Eq, Show, Enum, Bounded, Generic)

data LibraryPreset
  = LibraryPresetAll
  | LibraryPresetShortReads
  | LibraryPresetStandardReads
  | LibraryPresetLongReads
  | LibraryPresetNotUploaded
  | LibraryPresetDuplicateReview
  | LibraryPresetCustom
  deriving (Eq, Show, Enum, Bounded, Generic)

data RowDensity
  = CompactRows
  | ComfortableRows
  deriving (Eq, Show, Enum, Bounded, Generic)

data UiTheme
  = DarkUiTheme
  | LightUiTheme
  deriving (Eq, Show, Enum, Bounded, Generic)

data LibraryPage = LibraryPage
  { libraryPageArticles :: [ArticleSummary]
  , libraryPageTotal :: Int
  } deriving (Eq, Show, Generic)

defaultLibraryQuery :: LibraryQuery
defaultLibraryQuery =
  LibraryQuery
    { librarySearch = Nothing
    , librarySection = Nothing
    , libraryWordFilter = WordFilter Nothing Nothing
    , libraryIncludeIgnored = False
    , libraryOnlyIgnored = False
    , libraryOnlyNotUploaded = False
    , libraryOnlyDuplicateTitles = False
    , librarySort = LibrarySortNewest
    , libraryLimit = 30
    , libraryOffset = 0
    }

data LingqLesson = LingqLesson
  { lessonId :: Text
  , lessonUrl :: Text
  } deriving (Eq, Show, Generic)

data LingqRemoteLesson = LingqRemoteLesson
  { remoteLessonId :: Text
  , remoteLessonTitle :: Text
  , remoteLessonOriginalUrl :: Maybe Text
  , remoteLessonUrl :: Text
  } deriving (Eq, Show, Generic)

data LingqLanguage = LingqLanguage
  { languageCode :: Text
  , languageTitle :: Text
  } deriving (Eq, Show, Generic)

data LingqCollection = LingqCollection
  { collectionId :: Text
  , collectionTitle :: Text
  , collectionLessonsCount :: Int
  } deriving (Eq, Show, Generic)

data ArticleSummary = ArticleSummary
  { summaryId :: Maybe ArticleId
  , summaryUrl :: Text
  , summaryTitle :: Text
  , summarySection :: Text
  , summaryWordCount :: Int
  , summaryIgnored :: Bool
  , summaryUploaded :: Bool
  , summaryKnownPct :: Maybe Int
  } deriving (Eq, Show, Generic)

data Article = Article
  { articleId :: Maybe ArticleId
  , articleUrl :: Text
  , articleTitle :: Text
  , articleSubtitle :: Text
  , articleAuthor :: Text
  , articleDate :: Maybe Text
  , articleSection :: Text
  , articleParagraphs :: [Text]
  , articleFetchedAt :: Maybe UTCTime
  , articleUploadedLesson :: Maybe LingqLesson
  , articleIgnored :: Bool
  , articleAudioUrl :: Maybe Text
  , articleAudioPath :: Maybe FilePath
  , articleKnownPct :: Maybe Int
  } deriving (Eq, Show, Generic)

data LibraryStats = LibraryStats
  { totalArticles :: Int
  , uploadedArticles :: Int
  , averageWordCount :: Int
  , sectionCounts :: Map Text Int
  } deriving (Eq, Show, Generic)
