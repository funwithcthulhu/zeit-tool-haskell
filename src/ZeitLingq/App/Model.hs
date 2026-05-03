{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Model
  ( Model(..)
  , emptyAuth
  , initialModel
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import ZeitLingq.Domain.Types

data Model = Model
  { currentView :: View
  , zeitStatus :: AuthStatus
  , lingqStatus :: AuthStatus
  , selectedArticle :: Maybe ArticleSummary
  , selectedArticleContent :: Maybe Article
  , browseArticles :: [ArticleSummary]
  , libraryArticles :: [ArticleSummary]
  , lingqArticles :: [ArticleSummary]
  , notification :: Maybe Notification
  , browseSectionId :: Text
  , browseFilter :: WordFilter
  , libraryFilter :: WordFilter
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
    , zeitStatus = emptyAuth
    , lingqStatus = emptyAuth
    , selectedArticle = Nothing
    , selectedArticleContent = Nothing
    , browseArticles = []
    , libraryArticles = []
    , lingqArticles = []
    , notification = Nothing
    , browseSectionId = "index"
    , browseFilter = WordFilter Nothing Nothing
    , libraryFilter = WordFilter Nothing Nothing
    , lingqFilter = WordFilter Nothing Nothing
    , datePrefixEnabled = True
    , sectionCollections = Map.empty
    }
