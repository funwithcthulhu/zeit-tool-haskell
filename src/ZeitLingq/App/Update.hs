{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Update
  ( Command(..)
  , Event(..)
  , update
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
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
  | ArticleAudioDownloadRequested FilePath ArticleId
  | BrowseArticleHidden Text
  | BrowseBatchFetchRequested [ArticleSummary]
  | LingqBatchUploadRequested Day (Maybe Text) [ArticleSummary]
  | KnownWordsSyncRequested Text
  | BrowseArticlesLoaded [ArticleSummary]
  | LibraryArticlesLoaded [ArticleSummary]
  | LibraryPageLoaded LibraryPage
  | LingqArticlesLoaded [ArticleSummary]
  | RefreshCurrentView
  | Notify NotificationLevel Text
  | NotificationCleared
  | BrowseSectionSelected Text
  | BrowseFilterChanged WordFilter
  | LibraryFilterChanged WordFilter
  | LibrarySearchChanged Text
  | LibraryIncludeIgnoredChanged Bool
  | LibraryOnlyIgnoredChanged Bool
  | LibraryOnlyNotUploadedChanged Bool
  | LibraryPageChanged Int
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
  | RefreshLibraryPage LibraryQuery
  | RefreshLingqLibrary WordFilter
  | LoadArticle ArticleId
  | FetchAndSaveArticle ArticleSummary
  | DeleteSavedArticle ArticleId
  | SetArticleIgnored ArticleId Bool
  | UploadSavedArticle Day (Maybe Text) (Map Text Text) Bool ArticleId
  | SetBrowseUrlIgnored Text
  | FetchAndSaveArticles WordFilter [ArticleSummary]
  | UploadSavedArticles Day (Maybe Text) (Map Text Text) Bool [ArticleId]
  | DownloadArticleAudio FilePath ArticleId
  | SyncKnownWordsFromLingq Text
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
    ArticleAudioDownloadRequested audioDir ident ->
      ( model
      , [DownloadArticleAudio audioDir ident]
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
    KnownWordsSyncRequested languageCode ->
      ( model
      , [SyncKnownWordsFromLingq languageCode]
      )
    BrowseArticlesLoaded articles ->
      ( model {browseArticles = articles}
      , []
      )
    LibraryArticlesLoaded articles ->
      ( model {libraryArticles = articles, libraryTotal = length articles}
      , []
      )
    LibraryPageLoaded page ->
      ( model
          { libraryArticles = libraryPageArticles page
          , libraryTotal = libraryPageTotal page
          }
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
      let nextQuery =
            (libraryQuery model)
              { libraryWordFilter = filters
              , libraryOffset = 0
              }
       in ( model {libraryFilter = filters, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibrarySearchChanged search ->
      let nextQuery =
            (libraryQuery model)
              { librarySearch = nonEmptyText search
              , libraryOffset = 0
              }
       in ( model {libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryIncludeIgnoredChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryIncludeIgnored = enabled
              , libraryOnlyIgnored = libraryOnlyIgnored (libraryQuery model) && enabled
              , libraryOffset = 0
              }
       in ( model {libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryOnlyIgnoredChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryOnlyIgnored = enabled
              , libraryIncludeIgnored = enabled || libraryIncludeIgnored (libraryQuery model)
              , libraryOffset = 0
              }
       in ( model {libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryOnlyNotUploadedChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryOnlyNotUploaded = enabled
              , libraryOffset = 0
              }
       in ( model {libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryPageChanged offset ->
      let nextQuery =
            (libraryQuery model)
              { libraryOffset = max 0 offset
              }
       in ( model {libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
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
    LibraryView -> [RefreshLibraryPage (libraryQuery model)]
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

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
  | stripped == "" = Nothing
  | otherwise = Just stripped
  where
    stripped = stripText value

stripText :: Text -> Text
stripText = T.strip
