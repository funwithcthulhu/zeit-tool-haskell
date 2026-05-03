{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Update
  ( Command(..)
  , Event(..)
  , update
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, UTCTime)
import ZeitLingq.App.Model (Model(..))
import ZeitLingq.Domain.Types

data Event
  = Navigate View
  | ZeitStatusChanged AuthStatus
  | LingqStatusChanged AuthStatus
  | ZeitCookieChanged Text
  | ZeitCookieLoginRequested Text
  | ZeitLogoutRequested
  | LingqApiKeyChanged Text
  | LingqUsernameChanged Text
  | LingqPasswordChanged Text
  | LingqApiKeyLoginRequested Text
  | LingqPasswordLoginRequested Text Text
  | LingqLogoutRequested
  | ArticleOpened ArticleSummary
  | ArticleContentLoaded Article
  | ArticleClosed
  | BrowseArticlePreviewRequested ArticleSummary
  | BrowseArticleFetchRequested ArticleSummary
  | ArticleDeleteRequested ArticleId
  | ArticleIgnoredChanged ArticleId Bool
  | ArticleUploadRequested Day (Maybe Text) ArticleId
  | ArticleAudioDownloadRequested FilePath ArticleId
  | ArticleAudioOpenRequested ArticleId
  | BrowseArticleHidden Text
  | BrowseArticleUnhidden Text
  | BrowseSelectionToggled Text
  | BrowseSelectionChanged (Set Text)
  | BrowseShowHiddenChanged Bool
  | BrowseOnlyNewChanged Bool
  | BrowseSearchChanged Text
  | BrowseBatchFetchRequested [ArticleSummary]
  | LingqBatchUploadRequested Day (Maybe Text) [ArticleSummary]
  | LingqSelectionToggled ArticleId
  | LingqSelectionChanged (Set ArticleId)
  | KnownWordsSyncRequested Text
  | KnownWordsImportTextChanged Text
  | KnownWordsImportRequested Text Text Bool
  | KnownWordsComputeRequested Text
  | KnownWordsClearRequested Text
  | KnownWordsInfoLoaded Int
  | LingqCollectionsRefreshRequested Text
  | LingqCollectionsLoaded [LingqCollection]
  | LingqFallbackCollectionChanged Text
  | LingqOnlyNotUploadedChanged Bool
  | LingqKnownImportVisibilityChanged Bool
  | LingqSectionMappingsVisibilityChanged Bool
  | BrowseArticlesLoaded [ArticleSummary]
  | LibraryArticlesLoaded [ArticleSummary]
  | LibraryPageLoaded LibraryPage
  | LibraryStatsLoaded LibraryStats
  | LingqArticlesLoaded [ArticleSummary]
  | RefreshCurrentView
  | Notify NotificationLevel Text
  | NotificationCleared
  | BrowseSectionSelected Text
  | BrowsePageChanged Int
  | BrowseFilterChanged WordFilter
  | LibraryFilterChanged WordFilter
  | LibrarySearchChanged Text
  | LibrarySectionChanged Text
  | LibraryIncludeIgnoredChanged Bool
  | LibraryOnlyIgnoredChanged Bool
  | LibraryOnlyNotUploadedChanged Bool
  | LibraryGroupBySectionChanged Bool
  | LibraryPageChanged Int
  | LibraryDeleteIgnoredRequested
  | LibraryDeleteOlderRequested UTCTime Bool Bool
  | LingqFilterChanged WordFilter
  | DatePrefixToggled Bool
  | SectionCollectionsChanged (Map Text Text)
  deriving (Eq, Show)

data Command
  = PersistCurrentView View
  | LoginZeitWithCookie Text
  | LogoutZeit
  | LoginLingqWithApiKey Text
  | LoginLingqWithPassword Text Text
  | LogoutLingq
  | PersistBrowseSection Text
  | PersistBrowseFilter WordFilter
  | PersistDatePrefix Bool
  | PersistLingqFallbackCollection (Maybe Text)
  | PersistSectionCollections (Map Text Text)
  | RefreshBrowse Text Int Bool
  | RefreshLibrary WordFilter
  | RefreshLibraryPage LibraryQuery
  | LoadLibraryStats
  | RefreshLingqLibrary WordFilter Bool
  | LoadArticle ArticleId
  | PreviewArticle Text
  | FetchAndSaveArticle ArticleSummary
  | DeleteSavedArticle ArticleId
  | SetArticleIgnored ArticleId Bool
  | UploadSavedArticle Day (Maybe Text) (Map Text Text) Bool ArticleId
  | SetBrowseUrlIgnored Text
  | SetBrowseUrlUnignored Text
  | FetchAndSaveArticles WordFilter [ArticleSummary]
  | UploadSavedArticles Day (Maybe Text) (Map Text Text) Bool [ArticleId]
  | DownloadArticleAudio FilePath ArticleId
  | OpenArticleAudio ArticleId
  | SyncKnownWordsFromLingq Text
  | ImportKnownWordText Text Text Bool
  | ComputeKnownPercentagesFor Text
  | ClearKnownWords Text
  | LoadKnownWordsInfo Text
  | RefreshLingqCollections Text
  | DeleteIgnoredArticles
  | DeleteOlderArticles UTCTime Bool Bool
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
    ZeitCookieChanged cookie ->
      ( model {zeitCookieText = cookie}
      , []
      )
    ZeitCookieLoginRequested cookie ->
      ( model {zeitCookieText = cookie}
      , [LoginZeitWithCookie cookie]
      )
    ZeitLogoutRequested ->
      ( model
      , [LogoutZeit]
      )
    LingqApiKeyChanged apiKey ->
      ( model {lingqApiKeyText = apiKey}
      , []
      )
    LingqUsernameChanged username ->
      ( model {lingqUsernameText = username}
      , []
      )
    LingqPasswordChanged password ->
      ( model {lingqPasswordText = password}
      , []
      )
    LingqApiKeyLoginRequested apiKey ->
      ( model {lingqApiKeyText = apiKey}
      , [LoginLingqWithApiKey apiKey]
      )
    LingqPasswordLoginRequested username password ->
      ( model {lingqUsernameText = username, lingqPasswordText = password}
      , [LoginLingqWithPassword username password]
      )
    LingqLogoutRequested ->
      ( model
      , [LogoutLingq]
      )
    ArticleOpened article ->
      ( model
          { selectedArticle = Just article
          , selectedArticleContent = Nothing
          , currentView = ArticleView
          , articleReturnView = currentView model
          }
      , PersistCurrentView ArticleView : maybe [] (pure . LoadArticle) (summaryId article)
      )
    ArticleContentLoaded article ->
      ( model {selectedArticleContent = Just article}
      , []
      )
    ArticleClosed ->
      let returnView = articleReturnView model
          nextModel =
            model
              { selectedArticle = Nothing
              , selectedArticleContent = Nothing
              , currentView = returnView
              }
       in ( nextModel
          , PersistCurrentView returnView : refreshCommands nextModel
          )
    BrowseArticlePreviewRequested article ->
      let nextModel =
            model
              { selectedArticle = Just article
              , selectedArticleContent = Nothing
              , currentView = ArticleView
              , articleReturnView = BrowseView
              }
       in ( nextModel
          , [PersistCurrentView ArticleView, PreviewArticle (summaryUrl article)]
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
    ArticleAudioOpenRequested ident ->
      ( model
      , [OpenArticleAudio ident]
      )
    BrowseArticleHidden url ->
      ( model
      , [SetBrowseUrlIgnored url]
      )
    BrowseArticleUnhidden url ->
      ( model
      , [SetBrowseUrlUnignored url]
      )
    BrowseSelectionToggled url ->
      ( model {browseSelectedUrls = toggleSetMember url (browseSelectedUrls model)}
      , []
      )
    BrowseSelectionChanged urls ->
      ( model {browseSelectedUrls = urls}
      , []
      )
    BrowseShowHiddenChanged enabled ->
      let nextModel = model {browseShowHidden = enabled, browseSelectedUrls = Set.empty}
       in ( nextModel
          , [RefreshBrowse (browseSectionId nextModel) (browsePage nextModel) enabled]
          )
    BrowseOnlyNewChanged enabled ->
      ( model {browseOnlyNew = enabled, browseSelectedUrls = Set.empty}
      , []
      )
    BrowseSearchChanged search ->
      ( model {browseSearch = search, browseSelectedUrls = Set.empty}
      , []
      )
    BrowseBatchFetchRequested articles ->
      ( model
      , [FetchAndSaveArticles (browseFilter model) articles]
      )
    LingqBatchUploadRequested day fallbackCollection articles ->
      ( model
      , [UploadSavedArticles day fallbackCollection (sectionCollections model) (datePrefixEnabled model) (uploadableIds articles)]
      )
    LingqSelectionToggled ident ->
      ( model {lingqSelectedIds = toggleSetMember ident (lingqSelectedIds model)}
      , []
      )
    LingqSelectionChanged idents ->
      ( model {lingqSelectedIds = idents}
      , []
      )
    KnownWordsSyncRequested languageCode ->
      ( model
      , [SyncKnownWordsFromLingq languageCode]
      )
    KnownWordsImportTextChanged text ->
      ( model {knownImportText = text}
      , []
      )
    KnownWordsImportRequested languageCode text replaceExisting ->
      ( model
      , [ImportKnownWordText languageCode text replaceExisting]
      )
    KnownWordsComputeRequested languageCode ->
      ( model
      , [ComputeKnownPercentagesFor languageCode]
      )
    KnownWordsClearRequested languageCode ->
      ( model
      , [ClearKnownWords languageCode]
      )
    KnownWordsInfoLoaded total ->
      ( model {knownStemTotal = total}
      , []
      )
    LingqCollectionsRefreshRequested languageCode ->
      ( model
      , [RefreshLingqCollections languageCode]
      )
    LingqCollectionsLoaded collections ->
      ( model {lingqCollections = collections}
      , []
      )
    LingqFallbackCollectionChanged collectionId ->
      let nextCollection = nonEmptyText collectionId
       in ( model {lingqFallbackCollection = nextCollection}
          , [PersistLingqFallbackCollection nextCollection]
          )
    LingqOnlyNotUploadedChanged enabled ->
      ( model {lingqOnlyNotUploaded = enabled, lingqSelectedIds = Set.empty}
      , [RefreshLingqLibrary (lingqFilter model) enabled]
      )
    LingqKnownImportVisibilityChanged enabled ->
      ( model {lingqShowKnownImport = enabled}
      , []
      )
    LingqSectionMappingsVisibilityChanged enabled ->
      ( model {lingqShowSectionMappings = enabled}
      , []
      )
    BrowseArticlesLoaded articles ->
      ( model
          { browseArticles = articles
          , browseSelectedUrls = pruneBrowseSelection articles (browseSelectedUrls model)
          }
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
    LibraryStatsLoaded stats ->
      ( model {libraryStats = Just stats}
      , []
      )
    LingqArticlesLoaded articles ->
      ( model
          { lingqArticles = articles
          , lingqSelectedIds = pruneLingqSelection articles (lingqSelectedIds model)
          }
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
      ( model {browseSectionId = sectionIdValue, browsePage = 1, browseSelectedUrls = Set.empty}
      , [PersistBrowseSection sectionIdValue, RefreshBrowse sectionIdValue 1 (browseShowHidden model)]
      )
    BrowsePageChanged page ->
      let nextPage = max 1 page
       in ( model {browsePage = nextPage, browseSelectedUrls = Set.empty}
          , [RefreshBrowse (browseSectionId model) nextPage (browseShowHidden model)]
          )
    BrowseFilterChanged filters ->
      ( model {browseFilter = filters}
      , [PersistBrowseFilter filters]
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
    LibrarySectionChanged sectionName ->
      let nextQuery =
            (libraryQuery model)
              { librarySection = nonEmptyText sectionName
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
    LibraryGroupBySectionChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryLimit =
                  if enabled
                    then 5000
                    else libraryLimit defaultLibraryQuery
              , libraryOffset = 0
              }
       in ( model {libraryGroupBySection = enabled, libraryQuery = nextQuery}
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
    LibraryDeleteIgnoredRequested ->
      ( model
      , [DeleteIgnoredArticles]
      )
    LibraryDeleteOlderRequested cutoff onlyUploaded onlyUnuploaded ->
      ( model
      , [DeleteOlderArticles cutoff onlyUploaded onlyUnuploaded]
      )
    LingqFilterChanged filters ->
      ( model {lingqFilter = filters}
      , [RefreshLingqLibrary filters (lingqOnlyNotUploaded model)]
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
    BrowseView -> [RefreshBrowse (browseSectionId model) (browsePage model) (browseShowHidden model)]
    LibraryView -> [RefreshLibraryPage (libraryQuery model), LoadLibraryStats]
    LingqView ->
      [RefreshLingqLibrary (lingqFilter model) (lingqOnlyNotUploaded model), LoadKnownWordsInfo "de"]
        <> [RefreshLingqCollections "de" | authLoggedIn (lingqStatus model)]
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

toggleSetMember :: Ord a => a -> Set a -> Set a
toggleSetMember value values
  | Set.member value values = Set.delete value values
  | otherwise = Set.insert value values

pruneBrowseSelection :: [ArticleSummary] -> Set Text -> Set Text
pruneBrowseSelection articles selected =
  Set.intersection selected visibleUrls
  where
    visibleUrls = Set.fromList (map summaryUrl articles)

pruneLingqSelection :: [ArticleSummary] -> Set ArticleId -> Set ArticleId
pruneLingqSelection articles selected =
  Set.intersection selected visibleIds
  where
    visibleIds =
      Set.fromList
        [ ident
        | article <- articles
        , Just ident <- [summaryId article]
        ]
