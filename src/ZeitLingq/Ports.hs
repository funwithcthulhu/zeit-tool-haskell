{-# LANGUAGE RankNTypes #-}

module ZeitLingq.Ports
  ( AppPorts(..)
  , LibraryPort(..)
  , LingqPort(..)
  , SettingsPort(..)
  , ZeitPort(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import ZeitLingq.Domain.Types

data ZeitPort m = ZeitPort
  { fetchSections :: m [Section]
  , fetchArticleList :: Text -> Int -> m [ArticleSummary]
  , fetchArticleContent :: Text -> m Article
  , loginToZeit :: m AuthStatus
  , logoutFromZeit :: m ()
  }

data LingqPort m = LingqPort
  { loginToLingq :: Text -> Text -> m AuthStatus
  , logoutFromLingq :: m ()
  , uploadLessonToLingq :: Text -> Maybe Text -> Article -> m LingqLesson
  , fetchKnownWords :: Text -> m [Text]
  }

data LibraryPort m = LibraryPort
  { loadLibrary :: WordFilter -> m [ArticleSummary]
  , loadArticle :: ArticleId -> m (Maybe Article)
  , saveArticle :: Article -> m ArticleId
  , deleteArticle :: ArticleId -> m ()
  , loadStats :: m LibraryStats
  }

data SettingsPort m = SettingsPort
  { loadCurrentView :: m View
  , saveCurrentView :: View -> m ()
  , loadBrowseSection :: m Text
  , saveBrowseSection :: Text -> m ()
  , loadDatePrefixEnabled :: m Bool
  , saveDatePrefixEnabled :: Bool -> m ()
  , loadSectionCollections :: m (Map Text Text)
  , saveSectionCollections :: Map Text Text -> m ()
  }

data AppPorts m = AppPorts
  { zeitPort :: ZeitPort m
  , lingqPort :: LingqPort m
  , libraryPort :: LibraryPort m
  , settingsPort :: SettingsPort m
  }
