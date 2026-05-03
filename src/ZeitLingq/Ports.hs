{-# LANGUAGE RankNTypes #-}

module ZeitLingq.Ports
  ( AppPorts(..)
  , AudioPort(..)
  , LibraryPort(..)
  , LingqPort(..)
  , SettingsPort(..)
  , ZeitPort(..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import ZeitLingq.Domain.Types

data ZeitPort m = ZeitPort
  { fetchSections :: m [Section]
  , fetchArticleList :: Text -> Int -> m [ArticleSummary]
  , fetchArticleContent :: Text -> m Article
  , loginToZeit :: m AuthStatus
  , loginToZeitWithCookie :: Text -> m AuthStatus
  , logoutFromZeit :: m ()
  }

data LingqPort m = LingqPort
  { loginToLingq :: Text -> Text -> m AuthStatus
  , loginToLingqWithApiKey :: Text -> m AuthStatus
  , logoutFromLingq :: m ()
  , uploadLessonToLingq :: Text -> Maybe Text -> Article -> m LingqLesson
  , updateLessonOnLingq :: Text -> LingqLesson -> Article -> m LingqLesson
  , fetchLanguages :: m [LingqLanguage]
  , fetchCollections :: Text -> m [LingqCollection]
  , fetchCollectionLessons :: Text -> Text -> m [LingqRemoteLesson]
  , fetchKnownWords :: Text -> m [Text]
  }

data AudioPort m = AudioPort
  { downloadArticleAudioFile :: FilePath -> Article -> m FilePath
  , openAudioFile :: FilePath -> m ()
  }

data LibraryPort m = LibraryPort
  { loadLibrary :: WordFilter -> m [ArticleSummary]
  , loadLibraryPage :: LibraryQuery -> m LibraryPage
  , loadArticle :: ArticleId -> m (Maybe Article)
  , saveArticle :: Article -> m ArticleId
  , deleteArticle :: ArticleId -> m ()
  , setArticleIgnored :: ArticleId -> Bool -> m ()
  , markArticleUploaded :: ArticleId -> LingqLesson -> m ()
  , setArticleAudioPath :: ArticleId -> Maybe FilePath -> m ()
  , loadIgnoredUrls :: m [Text]
  , ignoreArticleUrl :: Text -> m ()
  , unignoreArticleUrl :: Text -> m ()
  , deleteIgnoredArticles :: m Int
  , deleteOlderArticles :: UTCTime -> Bool -> Bool -> m Int
  , replaceKnownWords :: Text -> Set Text -> m Int
  , addKnownWords :: Text -> Set Text -> m Int
  , clearKnownWords :: Text -> m ()
  , clearKnownPercentages :: m ()
  , computeKnownPercentages :: Text -> m (Either Text Int)
  , knownStemCount :: Text -> m Int
  , loadStats :: m LibraryStats
  }

data SettingsPort m = SettingsPort
  { loadCurrentView :: m View
  , saveCurrentView :: View -> m ()
  , loadZeitCookie :: m Text
  , saveZeitCookie :: Text -> m ()
  , loadLingqApiKey :: m Text
  , saveLingqApiKey :: Text -> m ()
  , loadLingqLanguage :: m Text
  , saveLingqLanguage :: Text -> m ()
  , loadBrowseSection :: m Text
  , saveBrowseSection :: Text -> m ()
  , loadBrowseFilter :: m WordFilter
  , saveBrowseFilter :: WordFilter -> m ()
  , loadBrowseOnlyNew :: m Bool
  , saveBrowseOnlyNew :: Bool -> m ()
  , loadLingqFilter :: m WordFilter
  , saveLingqFilter :: WordFilter -> m ()
  , loadLingqOnlyNotUploaded :: m Bool
  , saveLingqOnlyNotUploaded :: Bool -> m ()
  , loadDatePrefixEnabled :: m Bool
  , saveDatePrefixEnabled :: Bool -> m ()
  , loadLingqFallbackCollection :: m (Maybe Text)
  , saveLingqFallbackCollection :: Maybe Text -> m ()
  , loadSectionCollections :: m (Map Text Text)
  , saveSectionCollections :: Map Text Text -> m ()
  , loadRowDensity :: m RowDensity
  , saveRowDensity :: RowDensity -> m ()
  , loadUiTheme :: m UiTheme
  , saveUiTheme :: UiTheme -> m ()
  }

data AppPorts m = AppPorts
  { zeitPort :: ZeitPort m
  , lingqPort :: LingqPort m
  , audioPort :: AudioPort m
  , libraryPort :: LibraryPort m
  , settingsPort :: SettingsPort m
  }
