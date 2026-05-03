{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Update
  ( Command(..)
  , Event(..)
  , update
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  | ZeitUserAgentChanged Text
  | ZeitCookieLoginRequested Text
  | ZeitBrowserSessionLoginRequested Text Text
  | ZeitLogoutRequested
  | LingqApiKeyChanged Text
  | LingqUsernameChanged Text
  | LingqPasswordChanged Text
  | LingqLanguageChanged Text
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
  | LingqStatusSyncRequested Text Text
  | LingqSelectionToggled ArticleId
  | LingqSelectionChanged (Set ArticleId)
  | KnownWordsSyncRequested Text
  | KnownWordsImportTextChanged Text
  | KnownWordsImportRequested Text Text Bool
  | KnownWordsComputeRequested Text
  | KnownWordsClearRequested Text
  | KnownWordsInfoLoaded Int
  | LingqLanguagesRefreshRequested
  | LingqLanguagesLoaded [LingqLanguage]
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
  | BatchFetchFinished [(Text, Text)]
  | BatchUploadFinished [(ArticleId, Text)]
  | FetchJobQueued Text [ArticleSummary]
  | UploadJobQueued Text [ArticleSummary]
  | QueuedJobStarted QueuedJob
  | CompletedJobRecorded CompletedJob
  | JobQueuePausedChanged Bool
  | QueuedJobsCleared
  | CompletedJobsCleared
  | FailureListsCleared
  | ProgressChanged (Maybe ProgressStatus)
  | RowDensityChanged RowDensity
  | UiThemeChanged UiTheme
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
  | LibrarySortChanged LibrarySort
  | LibraryPresetChanged LibraryPreset
  | LibraryGroupBySectionChanged Bool
  | LibrarySectionCollapseToggled Text
  | LibraryDeleteDaysChanged Text
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
  | LoginZeitWithBrowserSession Text Text
  | LogoutZeit
  | LoginLingqWithApiKey Text
  | LoginLingqWithPassword Text Text
  | LogoutLingq
  | PersistBrowseSection Text
  | PersistBrowseFilter WordFilter
  | PersistBrowseOnlyNew Bool
  | PersistLingqLanguage Text
  | PersistLingqFilter WordFilter
  | PersistLingqOnlyNotUploaded Bool
  | PersistDatePrefix Bool
  | PersistLingqFallbackCollection (Maybe Text)
  | PersistSectionCollections (Map Text Text)
  | PersistRowDensity RowDensity
  | PersistUiTheme UiTheme
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
  | UploadSavedArticle Day Text (Maybe Text) (Map Text Text) Bool ArticleId
  | SetBrowseUrlIgnored Text
  | SetBrowseUrlUnignored Text
  | FetchAndSaveArticles WordFilter [ArticleSummary]
  | UploadSavedArticles Day Text (Maybe Text) (Map Text Text) Bool [ArticleId]
  | SyncLingqStatus Text Text
  | DownloadArticleAudio FilePath ArticleId
  | OpenArticleAudio ArticleId
  | SyncKnownWordsFromLingq Text
  | ImportKnownWordText Text Text Bool
  | ComputeKnownPercentagesFor Text
  | ClearKnownWords Text
  | LoadKnownWordsInfo Text
  | RefreshLingqLanguages
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
    ZeitUserAgentChanged userAgent ->
      ( model {zeitUserAgentText = userAgent}
      , []
      )
    ZeitCookieLoginRequested cookie ->
      ( model {zeitCookieText = cookie}
      , [LoginZeitWithCookie cookie]
      )
    ZeitBrowserSessionLoginRequested cookie userAgent ->
      ( model {zeitCookieText = cookie, zeitUserAgentText = userAgent}
      , [LoginZeitWithBrowserSession cookie userAgent]
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
    LingqLanguageChanged languageCode ->
      let nextLanguage = normalizeLanguageCode languageCode
          changed = nextLanguage /= lingqLanguage model
          nextModel =
            model
              { lingqLanguage = nextLanguage
              , lingqCollections = if changed then [] else lingqCollections model
              , lingqFallbackCollection = if changed then Nothing else lingqFallbackCollection model
              , sectionCollections = if changed then Map.empty else sectionCollections model
              , knownStemTotal = if changed then 0 else knownStemTotal model
              }
          resetCommands =
            if changed
              then [PersistLingqFallbackCollection Nothing, PersistSectionCollections Map.empty]
              else []
       in ( nextModel
          , [PersistLingqLanguage nextLanguage, LoadKnownWordsInfo nextLanguage]
              <> resetCommands
              <> [RefreshLingqCollections nextLanguage | authLoggedIn (lingqStatus model)]
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
      , [UploadSavedArticle day (lingqLanguage model) fallbackCollection (sectionCollections model) (datePrefixEnabled model) ident]
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
      , [PersistBrowseOnlyNew enabled]
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
      , [UploadSavedArticles day (lingqLanguage model) fallbackCollection (sectionCollections model) (datePrefixEnabled model) (uploadableIds articles)]
      )
    LingqStatusSyncRequested languageCode collectionId ->
      ( model
      , [SyncLingqStatus languageCode collectionId]
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
    LingqLanguagesRefreshRequested ->
      ( model
      , [RefreshLingqLanguages]
      )
    LingqLanguagesLoaded languages ->
      ( model {lingqLanguages = languagesWithCurrent (lingqLanguage model) languages}
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
      , [PersistLingqOnlyNotUploaded enabled, RefreshLingqLibrary (lingqFilter model) enabled]
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
    BatchFetchFinished failures ->
      ( model {failedFetches = failures, activeProgress = Nothing}
      , []
      )
    BatchUploadFinished failures ->
      ( model {failedUploads = failures, activeProgress = Nothing}
      , []
      )
    FetchJobQueued label articles ->
      if null articles
        then (model {notification = Just (Notification ErrorNotice "No articles to queue.")}, [])
        else
          let job = QueuedFetchJob (nextJobId model) label (browseFilter model) articles
           in ( model
                  { queuedJobs = queuedJobs model <> [job]
                  , nextJobId = nextJobId model + 1
                  , notification = Just (Notification InfoNotice ("Queued: " <> label))
                  }
              , []
              )
    UploadJobQueued label articles ->
      if null articles
        then (model {notification = Just (Notification ErrorNotice "No articles to queue.")}, [])
        else
          let job = QueuedUploadJob (nextJobId model) label articles
           in ( model
                  { queuedJobs = queuedJobs model <> [job]
                  , nextJobId = nextJobId model + 1
                  , notification = Just (Notification InfoNotice ("Queued: " <> label))
                  }
              , []
              )
    QueuedJobStarted job ->
      ( model {queuedJobs = filter ((/= queuedJobId job) . queuedJobId) (queuedJobs model)}
      , []
      )
    CompletedJobRecorded job ->
      ( model {completedJobs = take 30 (job : completedJobs model)}
      , []
      )
    JobQueuePausedChanged paused ->
      ( model
          { jobQueuePaused = paused
          , notification = Just (Notification InfoNotice (if paused then "Job queue paused." else "Job queue resumed."))
          }
      , []
      )
    QueuedJobsCleared ->
      ( model
          { queuedJobs = []
          , notification = Just (Notification InfoNotice "Cleared queued jobs.")
          }
      , []
      )
    CompletedJobsCleared ->
      ( model
          { completedJobs = []
          , notification = Just (Notification InfoNotice "Cleared completed job history.")
          }
      , []
      )
    FailureListsCleared ->
      ( model {failedFetches = [], failedUploads = []}
      , []
      )
    ProgressChanged progress ->
      ( model {activeProgress = progress}
      , []
      )
    RowDensityChanged density ->
      ( model {rowDensity = density}
      , [PersistRowDensity density]
      )
    UiThemeChanged theme ->
      ( model {uiTheme = theme}
      , [PersistUiTheme theme]
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
       in ( model {libraryFilter = filters, libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibrarySearchChanged search ->
      let nextQuery =
            (libraryQuery model)
              { librarySearch = nonEmptyText search
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibrarySectionChanged sectionName ->
      let nextQuery =
            (libraryQuery model)
              { librarySection = nonEmptyText sectionName
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryIncludeIgnoredChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryIncludeIgnored = enabled
              , libraryOnlyIgnored = libraryOnlyIgnored (libraryQuery model) && enabled
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryOnlyIgnoredChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryOnlyIgnored = enabled
              , libraryIncludeIgnored = enabled || libraryIncludeIgnored (libraryQuery model)
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryOnlyNotUploadedChanged enabled ->
      let nextQuery =
            (libraryQuery model)
              { libraryOnlyNotUploaded = enabled
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibrarySortChanged sortMode ->
      let nextQuery =
            (libraryQuery model)
              { librarySort = sortMode
              , libraryOffset = 0
              }
       in ( model {libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibraryPresetChanged preset ->
      let nextQuery =
            if preset == LibraryPresetCustom
              then (libraryQuery model) {libraryOffset = 0}
              else libraryQueryForPreset preset
       in ( model
              { libraryPreset = preset
              , libraryFilter = libraryWordFilter nextQuery
              , libraryGroupBySection = False
              , libraryQuery = nextQuery
              }
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
       in ( model {libraryGroupBySection = enabled, libraryPreset = LibraryPresetCustom, libraryQuery = nextQuery}
          , [RefreshLibraryPage nextQuery]
          )
    LibrarySectionCollapseToggled sectionName ->
      ( model {libraryCollapsedSections = toggleSetMember sectionName (libraryCollapsedSections model)}
      , []
      )
    LibraryDeleteDaysChanged daysText ->
      ( model {libraryDeleteDaysText = daysText}
      , []
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
      , [PersistLingqFilter filters, RefreshLingqLibrary filters (lingqOnlyNotUploaded model)]
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
      [RefreshLingqLibrary (lingqFilter model) (lingqOnlyNotUploaded model), LoadKnownWordsInfo (lingqLanguage model)]
        <> if authLoggedIn (lingqStatus model)
          then [RefreshLingqLanguages, RefreshLingqCollections (lingqLanguage model)]
          else []
    ZeitLoginView -> []
    DiagnosticsView -> []
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

normalizeLanguageCode :: Text -> Text
normalizeLanguageCode value
  | T.null stripped = "de"
  | otherwise = stripped
  where
    stripped = T.toLower (T.strip value)

languagesWithCurrent :: Text -> [LingqLanguage] -> [LingqLanguage]
languagesWithCurrent current languages
  | any ((== current) . languageCode) languages = languages
  | otherwise = LingqLanguage current current : languages

libraryQueryForPreset :: LibraryPreset -> LibraryQuery
libraryQueryForPreset preset =
  case preset of
    LibraryPresetAll ->
      defaultLibraryQuery
    LibraryPresetShortReads ->
      defaultLibraryQuery
        { libraryWordFilter = WordFilter (Just 300) (Just 900)
        , libraryOnlyNotUploaded = True
        }
    LibraryPresetStandardReads ->
      defaultLibraryQuery
        { libraryWordFilter = WordFilter (Just 900) (Just 1800)
        , libraryOnlyNotUploaded = True
        }
    LibraryPresetLongReads ->
      defaultLibraryQuery
        { libraryWordFilter = WordFilter (Just 1800) Nothing
        , libraryOnlyNotUploaded = True
        }
    LibraryPresetNotUploaded ->
      defaultLibraryQuery {libraryOnlyNotUploaded = True}
    LibraryPresetDuplicateReview ->
      defaultLibraryQuery
        { libraryIncludeIgnored = True
        , libraryOnlyDuplicateTitles = True
        , librarySort = LibrarySortTitle
        }
    LibraryPresetCustom ->
      defaultLibraryQuery

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
