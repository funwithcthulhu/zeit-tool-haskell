{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Update
  ( Command(..)
  , Event(..)
  , update
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import ZeitLingq.App.Model (Model(..))
import ZeitLingq.Domain.Types

data Event
  = Navigate View
  | ZeitStatusChanged AuthStatus
  | LingqStatusChanged AuthStatus
  | ArticleOpened ArticleSummary
  | ArticleContentLoaded Article
  | ArticleClosed
  | BrowseArticlesLoaded [ArticleSummary]
  | LibraryArticlesLoaded [ArticleSummary]
  | LingqArticlesLoaded [ArticleSummary]
  | Notify NotificationLevel Text
  | NotificationCleared
  | BrowseSectionSelected Text
  | BrowseFilterChanged WordFilter
  | LibraryFilterChanged WordFilter
  | LingqFilterChanged WordFilter
  | DatePrefixToggled Bool
  | SectionCollectionsChanged (Map Text Text)
  deriving (Eq, Show)

data Command
  = PersistCurrentView View
  | PersistBrowseSection Text
  | PersistDatePrefix Bool
  | PersistSectionCollections (Map Text Text)
  | RefreshBrowse Text Int
  | RefreshLibrary WordFilter
  | RefreshLingqLibrary WordFilter
  | LoadArticle ArticleId
  deriving (Eq, Show)

update :: Event -> Model -> (Model, [Command])
update event model =
  case event of
    Navigate nextView ->
      ( model {currentView = nextView}
      , [PersistCurrentView nextView]
      )
    ZeitStatusChanged status ->
      ( model {zeitStatus = status}
      , []
      )
    LingqStatusChanged status ->
      ( model {lingqStatus = status}
      , []
      )
    ArticleOpened article ->
      ( model
          { selectedArticle = Just article
          , selectedArticleContent = Nothing
          , currentView = ArticleView
          }
      , PersistCurrentView ArticleView : maybe [] (pure . LoadArticle) (summaryId article)
      )
    ArticleContentLoaded article ->
      ( model {selectedArticleContent = Just article}
      , []
      )
    ArticleClosed ->
      ( model
          { selectedArticle = Nothing
          , selectedArticleContent = Nothing
          , currentView = LibraryView
          }
      , [PersistCurrentView LibraryView]
      )
    BrowseArticlesLoaded articles ->
      ( model {browseArticles = articles}
      , []
      )
    LibraryArticlesLoaded articles ->
      ( model {libraryArticles = articles}
      , []
      )
    LingqArticlesLoaded articles ->
      ( model {lingqArticles = articles}
      , []
      )
    Notify level message ->
      ( model {notification = Just (Notification level message)}
      , []
      )
    NotificationCleared ->
      ( model {notification = Nothing}
      , []
      )
    BrowseSectionSelected sectionIdValue ->
      ( model {browseSectionId = sectionIdValue}
      , [PersistBrowseSection sectionIdValue, RefreshBrowse sectionIdValue 1]
      )
    BrowseFilterChanged filters ->
      ( model {browseFilter = filters}
      , [RefreshBrowse (browseSectionId model) 1]
      )
    LibraryFilterChanged filters ->
      ( model {libraryFilter = filters}
      , [RefreshLibrary filters]
      )
    LingqFilterChanged filters ->
      ( model {lingqFilter = filters}
      , [RefreshLingqLibrary filters]
      )
    DatePrefixToggled enabled ->
      ( model {datePrefixEnabled = enabled}
      , [PersistDatePrefix enabled]
      )
    SectionCollectionsChanged mappings ->
      ( model {sectionCollections = mappings}
      , [PersistSectionCollections mappings]
      )
