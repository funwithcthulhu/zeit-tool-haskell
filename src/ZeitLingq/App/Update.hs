{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Update
  ( Command(..)
  , Event(..)
  , update
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (Day)
import ZeitLingq.App.Model (Model(..))
import ZeitLingq.Domain.Types

data Event
  = Navigate View
  | ZeitStatusChanged AuthStatus
  | LingqStatusChanged AuthStatus
  | ArticleOpened ArticleSummary
  | ArticleContentLoaded Article
  | ArticleClosed
  | BrowseArticleFetchRequested ArticleSummary
  | ArticleDeleteRequested ArticleId
  | ArticleIgnoredChanged ArticleId Bool
  | ArticleUploadRequested Day (Maybe Text) ArticleId
  | BrowseArticleHidden Text
  | BrowseBatchFetchRequested [ArticleSummary]
  | LingqBatchUploadRequested Day (Maybe Text) [ArticleSummary]
  | BrowseArticlesLoaded [ArticleSummary]
  | LibraryArticlesLoaded [ArticleSummary]
  | LingqArticlesLoaded [ArticleSummary]
  | RefreshCurrentView
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
  | FetchAndSaveArticle ArticleSummary
  | DeleteSavedArticle ArticleId
  | SetArticleIgnored ArticleId Bool
  | UploadSavedArticle Day (Maybe Text) (Map Text Text) Bool ArticleId
  | SetBrowseUrlIgnored Text
  | FetchAndSaveArticles WordFilter [ArticleSummary]
  | UploadSavedArticles Day (Maybe Text) (Map Text Text) Bool [ArticleId]
  deriving (Eq, Show)

update :: Event -> Model -> (Model, [Command])
update event model =
  case event of
    Navigate nextView ->
      let nextModel = model {currentView = nextView}
       in ( nextModel
          , PersistCurrentView nextView : refreshCommands nextModel
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
      let nextModel =
            model
              { selectedArticle = Nothing
              , selectedArticleContent = Nothing
              , currentView = LibraryView
              }
       in ( nextModel
          , PersistCurrentView LibraryView : refreshCommands nextModel
          )
    BrowseArticleFetchRequested article ->
      ( model
      , [FetchAndSaveArticle article]
      )
    ArticleDeleteRequested ident ->
      ( model
      , [DeleteSavedArticle ident]
      )
    ArticleIgnoredChanged ident ignored ->
      ( model
      , [SetArticleIgnored ident ignored]
      )
    ArticleUploadRequested day fallbackCollection ident ->
      ( model
      , [UploadSavedArticle day fallbackCollection (sectionCollections model) (datePrefixEnabled model) ident]
      )
    BrowseArticleHidden url ->
      ( model
      , [SetBrowseUrlIgnored url]
      )
    BrowseBatchFetchRequested articles ->
      ( model
      , [FetchAndSaveArticles (browseFilter model) articles]
      )
    LingqBatchUploadRequested day fallbackCollection articles ->
      ( model
      , [UploadSavedArticles day fallbackCollection (sectionCollections model) (datePrefixEnabled model) (uploadableIds articles)]
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
    RefreshCurrentView ->
      ( model
      , refreshCommands model
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

refreshCommands :: Model -> [Command]
refreshCommands model =
  case currentView model of
    BrowseView -> [RefreshBrowse (browseSectionId model) 1]
    LibraryView -> [RefreshLibrary (libraryFilter model)]
    LingqView -> [RefreshLingqLibrary (lingqFilter model)]
    ZeitLoginView -> []
    ArticleView -> maybe [] (maybe [] (pure . LoadArticle) . summaryId) (selectedArticle model)

uploadableIds :: [ArticleSummary] -> [ArticleId]
uploadableIds articles =
  [ ident
  | article <- articles
  , Just ident <- [summaryId article]
  , not (summaryUploaded article)
  , not (summaryIgnored article)
  ]
