{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Domain.Types
  ( Article(..)
  , ArticleId(..)
  , ArticleSummary(..)
  , AuthStatus(..)
  , LibraryPage(..)
  , LibraryQuery(..)
  , LibraryStats(..)
  , LingqCollection(..)
  , LingqLanguage(..)
  , LingqLesson(..)
  , Notification(..)
  , NotificationLevel(..)
  , Section(..)
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
  , libraryLimit :: Int
  , libraryOffset :: Int
  } deriving (Eq, Show, Generic)

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
    , libraryLimit = 30
    , libraryOffset = 0
    }

data LingqLesson = LingqLesson
  { lessonId :: Text
  , lessonUrl :: Text
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
