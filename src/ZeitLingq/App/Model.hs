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
  , lingqApiKeyText :: Text
  , lingqUsernameText :: Text
  , lingqPasswordText :: Text
  , selectedArticle :: Maybe ArticleSummary
  , selectedArticleContent :: Maybe Article
  , browseArticles :: [ArticleSummary]
  , browseSelectedUrls :: Set Text
  , browseShowHidden :: Bool
  , libraryArticles :: [ArticleSummary]
  , libraryTotal :: Int
  , libraryStats :: Maybe LibraryStats
  , libraryGroupBySection :: Bool
  , lingqArticles :: [ArticleSummary]
  , lingqCollections :: [LingqCollection]
  , lingqFallbackCollection :: Maybe Text
  , knownStemTotal :: Int
  , knownImportText :: Text
  , notification :: Maybe Notification
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
    , lingqApiKeyText = ""
    , lingqUsernameText = ""
    , lingqPasswordText = ""
    , selectedArticle = Nothing
    , selectedArticleContent = Nothing
    , browseArticles = []
    , browseSelectedUrls = Set.empty
    , browseShowHidden = False
    , libraryArticles = []
    , libraryTotal = 0
    , libraryStats = Nothing
    , libraryGroupBySection = False
    , lingqArticles = []
    , lingqCollections = []
    , lingqFallbackCollection = Nothing
    , knownStemTotal = 0
    , knownImportText = ""
    , notification = Nothing
    , browseSectionId = "index"
    , browsePage = 1
    , browseFilter = WordFilter Nothing Nothing
    , libraryFilter = WordFilter Nothing Nothing
    , libraryQuery = defaultLibraryQuery
    , lingqFilter = WordFilter Nothing Nothing
    , datePrefixEnabled = True
    , sectionCollections = Map.empty
    }
