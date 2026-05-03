{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Monomer hiding (Model)
import Monomer qualified as M
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO (hClose)
import System.Process (CreateProcess(std_in), StdStream(CreatePipe), callProcess, proc, readCreateProcessWithExitCode, waitForProcess, withCreateProcess)
import Data.Text.IO qualified as TIO
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.Core.Batch (BatchFetchResult(..))
import ZeitLingq.Core.Upload (BatchUploadConfig(..), BatchUploadResult(..), targetCollectionFor)
import ZeitLingq.Domain.Article (BatchDecision(..), applyWordFilter, lessonTitle)
import ZeitLingq.App.Driver (dispatchEvent, dispatchEvents)
import ZeitLingq.App.Model (Model(..))
import ZeitLingq.App.Startup (loadInitialModel)
import ZeitLingq.App.Update (Event(..), update)
import ZeitLingq.App.ViewModel
import ZeitLingq.Domain.Section (allSections)
import ZeitLingq.Domain.Types
import ZeitLingq.Infrastructure.Audio
import ZeitLingq.Infrastructure.Lingq
import ZeitLingq.Infrastructure.Settings
import ZeitLingq.Infrastructure.Sqlite
import ZeitLingq.Infrastructure.Zeit
import ZeitLingq.Ports

data BrowserZeitSession = BrowserZeitSession
  { browserCookieHeader :: Text
  , browserUserAgent :: Text
  } deriving (Eq, Show)

data GuiRuntime = GuiRuntime
  { guiPorts :: AppPorts IO
  , guiCancelFlag :: IORef Bool
  }

instance FromJSON BrowserZeitSession where
  parseJSON =
    withObject "BrowserZeitSession" $ \obj ->
      BrowserZeitSession
        <$> obj .: "cookieHeader"
        <*> obj .: "userAgent"

data GuiEvent
  = GuiInit
  | GuiModelLoaded Model
  | GuiFailed Text
  | GuiNavigate View
  | GuiRefresh
  | GuiZeitCookieChanged Text
  | GuiOpenZeitLoginPage
  | GuiZeitBrowserLogin
  | GuiZeitCookieLogin
  | GuiZeitLogout
  | GuiLingqApiKeyChanged Text
  | GuiLingqUsernameChanged Text
  | GuiLingqPasswordChanged Text
  | GuiLingqLanguageChanged Text
  | GuiLingqApiKeyLogin
  | GuiLingqPasswordLogin
  | GuiLingqLogout
  | GuiSectionSelected Text
  | GuiBrowsePreviousPage
  | GuiBrowseNextPage
  | GuiBrowseMinWordsChanged Text
  | GuiBrowseMaxWordsChanged Text
  | GuiBrowseShowHiddenChanged Bool
  | GuiBrowseOnlyNewChanged Bool
  | GuiBrowseSearchChanged Text
  | GuiBrowseToggleSelection Text
  | GuiBrowseSelectVisible [ArticleSummary]
  | GuiBrowseClearSelection
  | GuiPreviewArticle ArticleSummary
  | GuiOpenArticle ArticleSummary
  | GuiFetchArticle ArticleSummary
  | GuiHideBrowseArticle ArticleSummary
  | GuiUnhideBrowseArticle ArticleSummary
  | GuiFetchSelected [ArticleSummary]
  | GuiFetchVisible [ArticleSummary]
  | GuiToggleIgnored ArticleSummary
  | GuiUploadArticle ArticleId
  | GuiLingqToggleSelection ArticleId
  | GuiLingqSelectNotUploaded [ArticleSummary]
  | GuiLingqClearSelection
  | GuiUploadSelected [ArticleSummary]
  | GuiUploadVisible [ArticleSummary]
  | GuiSyncLingqStatus
  | GuiDownloadAudio ArticleId
  | GuiOpenAudio ArticleId
  | GuiOpenExternal Text
  | GuiCopyText Text
  | GuiRetryFailedFetches
  | GuiRetryFailedUploads
  | GuiClearFailures
  | GuiQueuePausedChanged Bool
  | GuiClearQueuedJobs
  | GuiClearCompletedJobs
  | GuiRunNextQueuedJob
  | GuiCancelCurrentJob
  | GuiCancelArmed
  | GuiCopyRecentLog
  | GuiProgress (Maybe ProgressStatus)
  | GuiRowDensityChanged RowDensity
  | GuiUiThemeChanged UiTheme
  | GuiSyncKnownWords
  | GuiKnownImportTextChanged Text
  | GuiImportKnownWords
  | GuiComputeKnownWords
  | GuiClearKnownWords
  | GuiRefreshLanguages
  | GuiRefreshCollections
  | GuiFallbackCollectionChanged Text
  | GuiLingqMinWordsChanged Text
  | GuiLingqMaxWordsChanged Text
  | GuiLingqOnlyNotUploadedChanged Bool
  | GuiLingqKnownImportVisible Bool
  | GuiLingqMappingsVisible Bool
  | GuiDatePrefixChanged Bool
  | GuiSectionCollectionChanged Text Text
  | GuiLibrarySearchChanged Text
  | GuiLibrarySectionChanged Text
  | GuiLibraryMinWordsChanged Text
  | GuiLibraryMaxWordsChanged Text
  | GuiLibraryIncludeIgnoredChanged Bool
  | GuiLibraryOnlyIgnoredChanged Bool
  | GuiLibraryOnlyNotUploadedChanged Bool
  | GuiLibrarySortChanged LibrarySort
  | GuiLibraryPresetChanged LibraryPreset
  | GuiLibraryGroupBySectionChanged Bool
  | GuiLibraryDeleteDaysChanged Text
  | GuiLibraryPreviousPage
  | GuiLibraryNextPage
  | GuiDeleteIgnoredArticles
  | GuiDeleteOldArticles Bool Bool
  | GuiDeleteArticle ArticleId
  | GuiOpenDataFolder
  | GuiOpenLogs
  | GuiCreateSupportBundle
  | GuiCloseArticle
  | GuiClearNotice
  deriving (Eq, Show)

dbPath :: FilePath
dbPath = "zeit-tool.db"

settingsPath :: FilePath
settingsPath = "settings.json"

logPath :: FilePath
logPath = "logs/app.log"

zeitLoginUrl :: Text
zeitLoginUrl = "https://meine.zeit.de/anmelden?url=https%3A%2F%2Fwww.zeit.de%2Findex&entry_service=sonstige"

data Palette = Palette
  { paletteAppBg :: Color
  , palettePanelBg :: Color
  , palettePanelAlt :: Color
  , paletteBorder :: Color
  , palettePrimary :: Color
  , palettePrimaryText :: Color
  , paletteMainText :: Color
  , paletteMutedText :: Color
  , paletteWarning :: Color
  , paletteDanger :: Color
  , paletteDangerBg :: Color
  }

paletteFor :: UiTheme -> Palette
paletteFor DarkUiTheme =
  Palette
    { paletteAppBg = rgbHex "#0f151b"
    , palettePanelBg = rgbHex "#17212b"
    , palettePanelAlt = rgbHex "#101923"
    , paletteBorder = rgbHex "#263746"
    , palettePrimary = rgbHex "#16d9c5"
    , palettePrimaryText = rgbHex "#071113"
    , paletteMainText = rgbHex "#f3f7f8"
    , paletteMutedText = rgbHex "#9eadba"
    , paletteWarning = rgbHex "#f5a524"
    , paletteDanger = rgbHex "#ff6b6b"
    , paletteDangerBg = rgbHex "#3a1f24"
    }
paletteFor LightUiTheme =
  Palette
    { paletteAppBg = rgbHex "#f4efe6"
    , palettePanelBg = rgbHex "#fffaf2"
    , palettePanelAlt = rgbHex "#efe7da"
    , paletteBorder = rgbHex "#d6c8b5"
    , palettePrimary = rgbHex "#0f766e"
    , palettePrimaryText = rgbHex "#fbfffb"
    , paletteMainText = rgbHex "#17211d"
    , paletteMutedText = rgbHex "#66736d"
    , paletteWarning = rgbHex "#a16207"
    , paletteDanger = rgbHex "#c2410c"
    , paletteDangerBg = rgbHex "#fff0e8"
    }

palette :: Model -> Palette
palette = paletteFor . uiTheme

appBgColor :: Model -> Color
appBgColor = paletteAppBg . palette

panelBgColor :: Model -> Color
panelBgColor = palettePanelBg . palette

panelAltColor :: Model -> Color
panelAltColor = palettePanelAlt . palette

borderColor :: Model -> Color
borderColor = paletteBorder . palette

primaryColor :: Model -> Color
primaryColor = palettePrimary . palette

primaryTextColor :: Model -> Color
primaryTextColor = palettePrimaryText . palette

mainTextColor :: Model -> Color
mainTextColor = paletteMainText . palette

mutedTextColor :: Model -> Color
mutedTextColor = paletteMutedText . palette

warningColor :: Model -> Color
warningColor = paletteWarning . palette

dangerColor :: Model -> Color
dangerColor = paletteDanger . palette

dangerBgColor :: Model -> Color
dangerBgColor = paletteDangerBg . palette

monomerThemeFor :: UiTheme -> Theme
monomerThemeFor DarkUiTheme = darkTheme
monomerThemeFor LightUiTheme = lightTheme

buildUI :: WidgetEnv Model GuiEvent -> Model -> WidgetNode Model GuiEvent
buildUI _ model =
  themeSwitch_ (monomerThemeFor (uiTheme model)) [themeClearBg] $
    hstack
      [ sidebarBlock model vm
      , vstack
          [ titleBlock model vm
          , notificationBlock model
          , progressNoticeBlock model
          , contentBlock model vm
          ]
          `styleBasic` [padding 12]
      ]
      `styleBasic` [bgColor (appBgColor model)]
  where
    vm = appViewModel model

handleEvent
  :: GuiRuntime
  -> WidgetEnv Model GuiEvent
  -> WidgetNode Model GuiEvent
  -> Model
  -> GuiEvent
  -> [AppEventResponse Model GuiEvent]
handleEvent runtime _ _ model event =
  case event of
    GuiInit ->
      [Task (loadGuiInitialModel ports)]
    GuiModelLoaded nextModel ->
      modelLoadedResponses runtime model nextModel
    GuiFailed message ->
      [M.Model model {notification = Just (Notification ErrorNotice message)}]
    GuiNavigate view ->
      [Task (runAppEvent ports model (Navigate view))]
    GuiRefresh ->
      withPendingNotice model "Refreshing current view..." (runAppEvent ports model RefreshCurrentView)
    GuiZeitCookieChanged cookie ->
      [Task (runAppEvent ports model (ZeitCookieChanged cookie))]
    GuiOpenZeitLoginPage ->
      [Task (runSideEffect model (openExternalUrl zeitLoginUrl) "Opened Zeit login page in your browser.")]
    GuiZeitBrowserLogin ->
      withPendingNotice model "Opening browser-assisted Zeit login..." (runZeitBrowserLogin ports model)
    GuiZeitCookieLogin ->
      [Task (runAppEvent ports model (ZeitCookieLoginRequested (zeitCookieText model)))]
    GuiZeitLogout ->
      [Task (runAppEvent ports model ZeitLogoutRequested)]
    GuiLingqApiKeyChanged apiKey ->
      [Task (runAppEvent ports model (LingqApiKeyChanged apiKey))]
    GuiLingqUsernameChanged username ->
      [Task (runAppEvent ports model (LingqUsernameChanged username))]
    GuiLingqPasswordChanged password ->
      [Task (runAppEvent ports model (LingqPasswordChanged password))]
    GuiLingqLanguageChanged languageCode ->
      withPendingNotice model "Switching LingQ language..." (runAppEvent ports model (LingqLanguageChanged languageCode))
    GuiLingqApiKeyLogin ->
      [Task (runAppEvent ports model (LingqApiKeyLoginRequested (lingqApiKeyText model)))]
    GuiLingqPasswordLogin ->
      [Task (runAppEvent ports model (LingqPasswordLoginRequested (lingqUsernameText model) (lingqPasswordText model)))]
    GuiLingqLogout ->
      [Task (runAppEvent ports model LingqLogoutRequested)]
    GuiSectionSelected sectionIdent ->
      [Task (runAppEvent ports model (BrowseSectionSelected sectionIdent))]
    GuiBrowsePreviousPage ->
      [Task (runAppEvent ports model (BrowsePageChanged (browsePage model - 1)))]
    GuiBrowseNextPage ->
      [Task (runAppEvent ports model (BrowsePageChanged (browsePage model + 1)))]
    GuiBrowseMinWordsChanged raw ->
      [Task (runAppEvent ports model (BrowseFilterChanged ((browseFilter model) {minWords = parseOptionalInt raw})))]
    GuiBrowseMaxWordsChanged raw ->
      [Task (runAppEvent ports model (BrowseFilterChanged ((browseFilter model) {maxWords = parseOptionalInt raw})))]
    GuiBrowseShowHiddenChanged enabled ->
      [Task (runAppEvent ports model (BrowseShowHiddenChanged enabled))]
    GuiBrowseOnlyNewChanged enabled ->
      [Task (runAppEvent ports model (BrowseOnlyNewChanged enabled))]
    GuiBrowseSearchChanged search ->
      [Task (runAppEvent ports model (BrowseSearchChanged search))]
    GuiBrowseToggleSelection url ->
      [Task (runAppEvent ports model (BrowseSelectionToggled url))]
    GuiBrowseSelectVisible articles ->
      [Task (runAppEvent ports model (BrowseSelectionChanged (Set.fromList (map summaryUrl articles))))]
    GuiBrowseClearSelection ->
      [Task (runAppEvent ports model (BrowseSelectionChanged Set.empty))]
    GuiPreviewArticle article ->
      withPendingNotice model "Loading article preview..." (runAppEvent ports model (BrowseArticlePreviewRequested article))
    GuiOpenArticle article ->
      [Task (runAppEvent ports model (ArticleOpened article))]
    GuiFetchArticle article ->
      withPendingNotice model "Fetching and saving article..." (runAppEvent ports model (BrowseArticleFetchRequested article))
    GuiHideBrowseArticle article ->
      [Task (runAppEvent ports model (BrowseArticleHidden (summaryUrl article)))]
    GuiUnhideBrowseArticle article ->
      [Task (runAppEvent ports model (BrowseArticleUnhidden (summaryUrl article)))]
    GuiFetchSelected articles ->
      case selectedBrowseArticles model articles of
        [] -> [Task (runAppEvent ports model (Notify ErrorNotice "Select at least one article to fetch."))]
        selected -> queueOrStartFetchJob runtime model "Fetching selected articles" selected
    GuiFetchVisible articles ->
      queueOrStartFetchJob runtime model "Fetching visible articles" articles
    GuiToggleIgnored article ->
      case summaryId article of
        Just ident -> [Task (runAppEvent ports model (ArticleIgnoredChanged ident (not (summaryIgnored article))))]
        Nothing -> [Task (runAppEvent ports model (Notify ErrorNotice "Cannot ignore an unsaved article."))]
    GuiUploadArticle ident ->
      withPendingNotice model "Uploading article to LingQ..." (runUploadAppEvent ports model ident)
    GuiLingqToggleSelection ident ->
      [Task (runAppEvent ports model (LingqSelectionToggled ident))]
    GuiLingqSelectNotUploaded articles ->
      [Task (runAppEvent ports model (LingqSelectionChanged (Set.fromList (mapMaybeSummaryId (filter uploadableSummary articles)))))]
    GuiLingqClearSelection ->
      [Task (runAppEvent ports model (LingqSelectionChanged Set.empty))]
    GuiUploadSelected articles ->
      case selectedLingqArticles model articles of
        [] -> [Task (runAppEvent ports model (Notify ErrorNotice "Select at least one article to upload."))]
        selected -> queueOrStartUploadJob runtime model "Uploading selected articles" selected
    GuiUploadVisible articles ->
      queueOrStartUploadJob runtime model "Uploading visible articles" articles
    GuiSyncLingqStatus ->
      case lingqFallbackCollection model of
        Nothing -> [Task (runAppEvent ports model (Notify ErrorNotice "Choose a fallback LingQ course before syncing upload status."))]
        Just collectionId ->
          withPendingNotice
            model
            "Syncing local upload status from LingQ..."
            (runAppEvent ports model (LingqStatusSyncRequested (lingqLanguage model) collectionId))
    GuiDownloadAudio ident ->
      withPendingNotice model "Downloading article audio..." (runAppEvent ports model (ArticleAudioDownloadRequested "audio" ident))
    GuiOpenAudio ident ->
      [Task (runAppEvent ports model (ArticleAudioOpenRequested ident))]
    GuiOpenExternal url ->
      [Task (runSideEffect model (openExternalUrl url) "Opened original article.")]
    GuiCopyText text ->
      [Task (runSideEffect model (copyTextToClipboard text) "Copied article text to clipboard.")]
    GuiRetryFailedFetches ->
      case failedFetches model of
        [] -> [Task (runAppEvent ports model (Notify InfoNotice "No failed fetches to retry."))]
        failures ->
          queueOrStartFetchJob runtime model "Retrying failed fetches" (map failedFetchSummary failures)
    GuiRetryFailedUploads ->
      case failedUploads model of
        [] -> [Task (runAppEvent ports model (Notify InfoNotice "No failed uploads to retry."))]
        failures ->
          queueOrStartUploadJob runtime model "Retrying failed uploads" (map failedUploadSummary failures)
    GuiClearFailures ->
      [Task (runAppEvent ports model FailureListsCleared)]
    GuiQueuePausedChanged paused ->
      queueControlResponses runtime model (JobQueuePausedChanged paused)
    GuiClearQueuedJobs ->
      queueControlResponses runtime model QueuedJobsCleared
    GuiClearCompletedJobs ->
      queueControlResponses runtime model CompletedJobsCleared
    GuiRunNextQueuedJob ->
      startNextQueuedJobIfReady runtime (model {jobQueuePaused = False})
    GuiCancelCurrentJob ->
      if modelBusy model
        then [Task (writeIORef cancelFlag True >> pure GuiCancelArmed)]
        else [M.Model model {notification = Just (Notification InfoNotice "No running batch job to cancel.")}]
    GuiCancelArmed ->
      [ M.Model
          model
            { jobQueuePaused = True
            , notification = Just (Notification InfoNotice "Cancel requested. The queue is paused until you resume it.")
            }
      ]
    GuiCopyRecentLog ->
      [Task (runCopyRecentLog model)]
    GuiProgress progress ->
      let (nextModel, _) = update (ProgressChanged progress) model
       in [M.Model nextModel]
    GuiRowDensityChanged density ->
      [Task (runAppEvent ports model (RowDensityChanged density))]
    GuiUiThemeChanged theme ->
      [Task (runAppEvent ports model (UiThemeChanged theme))]
    GuiSyncKnownWords ->
      withPendingNotice model "Syncing known words from LingQ..." (runAppEvent ports model (KnownWordsSyncRequested (lingqLanguage model)))
    GuiKnownImportTextChanged text ->
      [Task (runAppEvent ports model (KnownWordsImportTextChanged text))]
    GuiImportKnownWords ->
      withPendingNotice model "Importing known words and updating estimates..." (runAppEvent ports model (KnownWordsImportRequested (lingqLanguage model) (knownImportText model) False))
    GuiComputeKnownWords ->
      withPendingNotice model "Refreshing known-word percentages..." (runAppEvent ports model (KnownWordsComputeRequested (lingqLanguage model)))
    GuiClearKnownWords ->
      withPendingNotice model "Clearing known words..." (runAppEvent ports model (KnownWordsClearRequested (lingqLanguage model)))
    GuiRefreshLanguages ->
      withPendingNotice model "Refreshing LingQ languages..." (runAppEvent ports model LingqLanguagesRefreshRequested)
    GuiRefreshCollections ->
      withPendingNotice model "Refreshing LingQ collections..." (runAppEvent ports model (LingqCollectionsRefreshRequested (lingqLanguage model)))
    GuiFallbackCollectionChanged collectionId ->
      [Task (runAppEvent ports model (LingqFallbackCollectionChanged collectionId))]
    GuiLingqMinWordsChanged raw ->
      [Task (runAppEvent ports model (LingqFilterChanged ((lingqFilter model) {minWords = parseOptionalInt raw})))]
    GuiLingqMaxWordsChanged raw ->
      [Task (runAppEvent ports model (LingqFilterChanged ((lingqFilter model) {maxWords = parseOptionalInt raw})))]
    GuiLingqOnlyNotUploadedChanged enabled ->
      [Task (runAppEvent ports model (LingqOnlyNotUploadedChanged enabled))]
    GuiLingqKnownImportVisible enabled ->
      [Task (runAppEvent ports model (LingqKnownImportVisibilityChanged enabled))]
    GuiLingqMappingsVisible enabled ->
      [Task (runAppEvent ports model (LingqSectionMappingsVisibilityChanged enabled))]
    GuiDatePrefixChanged enabled ->
      [Task (runAppEvent ports model (DatePrefixToggled enabled))]
    GuiSectionCollectionChanged sectionName collectionId ->
      [Task (runAppEvent ports model (SectionCollectionsChanged (updateSectionCollection sectionName collectionId (sectionCollections model))))]
    GuiLibrarySearchChanged search ->
      [Task (runAppEvent ports model (LibrarySearchChanged search))]
    GuiLibrarySectionChanged sectionName ->
      [Task (runAppEvent ports model (LibrarySectionChanged sectionName))]
    GuiLibraryMinWordsChanged raw ->
      [Task (runAppEvent ports model (LibraryFilterChanged ((libraryFilter model) {minWords = parseOptionalInt raw})))]
    GuiLibraryMaxWordsChanged raw ->
      [Task (runAppEvent ports model (LibraryFilterChanged ((libraryFilter model) {maxWords = parseOptionalInt raw})))]
    GuiLibraryIncludeIgnoredChanged enabled ->
      [Task (runAppEvent ports model (LibraryIncludeIgnoredChanged enabled))]
    GuiLibraryOnlyIgnoredChanged enabled ->
      [Task (runAppEvent ports model (LibraryOnlyIgnoredChanged enabled))]
    GuiLibraryOnlyNotUploadedChanged enabled ->
      [Task (runAppEvent ports model (LibraryOnlyNotUploadedChanged enabled))]
    GuiLibrarySortChanged sortMode ->
      [Task (runAppEvent ports model (LibrarySortChanged sortMode))]
    GuiLibraryPresetChanged preset ->
      [Task (runAppEvent ports model (LibraryPresetChanged preset))]
    GuiLibraryGroupBySectionChanged enabled ->
      [Task (runAppEvent ports model (LibraryGroupBySectionChanged enabled))]
    GuiLibraryDeleteDaysChanged daysText ->
      [Task (runAppEvent ports model (LibraryDeleteDaysChanged daysText))]
    GuiLibraryPreviousPage ->
      [Task (runAppEvent ports model (LibraryPageChanged (libraryOffset (libraryQuery model) - libraryLimit (libraryQuery model))))]
    GuiLibraryNextPage ->
      [Task (runAppEvent ports model (LibraryPageChanged (libraryOffset (libraryQuery model) + libraryLimit (libraryQuery model))))]
    GuiDeleteIgnoredArticles ->
      [Task (runAppEvent ports model LibraryDeleteIgnoredRequested)]
    GuiDeleteOldArticles onlyUploaded onlyUnuploaded ->
      case parsePositiveInt (libraryDeleteDaysText model) of
        Nothing -> [Task (runAppEvent ports model (Notify ErrorNotice "Enter a positive day count before deleting old articles."))]
        Just days -> [Task (runDeleteOlderAppEvent ports model days onlyUploaded onlyUnuploaded)]
    GuiDeleteArticle ident ->
      [Task (runAppEvent ports model (ArticleDeleteRequested ident))]
    GuiOpenDataFolder ->
      [Task (runSideEffect model (getCurrentDirectory >>= openExternalPath) "Opened project data folder.")]
    GuiOpenLogs ->
      [Task (runSideEffect model (ensureLogFile >> openExternalPath logPath) "Opened log file.")]
    GuiCreateSupportBundle ->
      [Task (runCreateSupportBundle model)]
    GuiCloseArticle ->
      [Task (runAppEvent ports model ArticleClosed)]
    GuiClearNotice ->
      [Task (runAppEvent ports model NotificationCleared)]
  where
    ports = guiPorts runtime
    cancelFlag = guiCancelFlag runtime

withPendingNotice :: Model -> Text -> IO GuiEvent -> [AppEventResponse Model GuiEvent]
withPendingNotice model message task =
  [ M.Model model {notification = Just (Notification InfoNotice message)}
  , Task task
  ]

modelBusy :: Model -> Bool
modelBusy model =
  activeProgress model /= Nothing

modelLoadedResponses :: GuiRuntime -> Model -> Model -> [AppEventResponse Model GuiEvent]
modelLoadedResponses runtime current loaded =
  startNextQueuedJobIfReady runtime (mergeLoadedModel current loaded)

mergeLoadedModel :: Model -> Model -> Model
mergeLoadedModel current loaded =
  loaded
    { queuedJobs = queuedJobs current
    , jobQueuePaused = jobQueuePaused current
    , nextJobId = max (nextJobId current) (nextJobId loaded)
    , completedJobs = mergeCompletedJobs (completedJobs loaded) (completedJobs current)
    }

mergeCompletedJobs :: [CompletedJob] -> [CompletedJob] -> [CompletedJob]
mergeCompletedJobs loaded current =
  take 30 (loaded <> filter isNew current)
  where
    loadedIds = Set.fromList (map completedJobId loaded)
    isNew job = not (Set.member (completedJobId job) loadedIds)

queueOrStartFetchJob :: GuiRuntime -> Model -> Text -> [ArticleSummary] -> [AppEventResponse Model GuiEvent]
queueOrStartFetchJob runtime model labelText articles
  | null articles = [Task (runAppEvent ports model (Notify ErrorNotice "No articles to fetch."))]
  | shouldQueueJob model = queueControlResponses runtime model (FetchJobQueued labelText articles)
  | otherwise = startQueuedJob runtime model (QueuedFetchJob (nextJobId model) labelText (browseFilter model) articles)
  where
    ports = guiPorts runtime

queueOrStartUploadJob :: GuiRuntime -> Model -> Text -> [ArticleSummary] -> [AppEventResponse Model GuiEvent]
queueOrStartUploadJob runtime model labelText articles
  | null articles = [Task (runAppEvent ports model (Notify ErrorNotice "No articles to upload."))]
  | shouldQueueJob model = queueControlResponses runtime model (UploadJobQueued labelText articles)
  | otherwise = startQueuedJob runtime model (QueuedUploadJob (nextJobId model) labelText articles)
  where
    ports = guiPorts runtime

shouldQueueJob :: Model -> Bool
shouldQueueJob model =
  modelBusy model || jobQueuePaused model || not (null (queuedJobs model))

queueControlResponses :: GuiRuntime -> Model -> Event -> [AppEventResponse Model GuiEvent]
queueControlResponses runtime model event =
  let (nextModel, _commands) = update event model
   in startNextQueuedJobIfReady runtime nextModel

startNextQueuedJobIfReady :: GuiRuntime -> Model -> [AppEventResponse Model GuiEvent]
startNextQueuedJobIfReady runtime model
  | modelBusy model || jobQueuePaused model = [M.Model model]
  | otherwise =
      case queuedJobs model of
        [] -> [M.Model model]
        job : _ -> startQueuedJob runtime model job

startQueuedJob :: GuiRuntime -> Model -> QueuedJob -> [AppEventResponse Model GuiEvent]
startQueuedJob runtime model job =
  let reservedModel = model {nextJobId = max (nextJobId model) (queuedJobId job + 1)}
      (dequeuedModel, _commands) = update (QueuedJobStarted job) reservedModel
      startModel =
        dequeuedModel
          { notification = Just (Notification InfoNotice ("Started: " <> queuedJobLabel job))
          , activeProgress = Just (ProgressStatus (queuedJobLabel job) 0 (queuedJobItemCount job) "")
          }
      producer =
        case job of
          QueuedFetchJob {} -> runFetchBatchProducer runtime startModel job
          QueuedUploadJob {} -> runUploadBatchProducer runtime startModel job
   in [M.Model startModel, Producer producer]

queuedJobItemCount :: QueuedJob -> Int
queuedJobItemCount QueuedFetchJob {queuedFetchArticles = articles} = length articles
queuedJobItemCount QueuedUploadJob {queuedUploadArticles = articles} = length articles

titleBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
titleBlock model vm =
  hstack
    [ label (vmTitle vm)
        `styleBasic` [textSize 18, textColor (mainTextColor model), paddingR 12]
    , filler
    , statusBlock model vm
    , label "Density"
        `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
    , textDropdownV_
        (rowDensity model)
        GuiRowDensityChanged
        allRowDensities
        rowDensityLabel
        []
        `styleBasic` (inputStyle model <> [width 126])
    , label "Theme"
        `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
    , textDropdownV_
        (uiTheme model)
        GuiUiThemeChanged
        allUiThemes
        uiThemeLabel
        []
        `styleBasic` (inputStyle model <> [width 92])
    , secondaryButton model "Refresh" GuiRefresh
    ]
    `styleBasic` [paddingB 6]

sidebarBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
sidebarBlock model vm =
  vstack
    [ label "Zeit Reader"
        `styleBasic` [textSize 18, textColor (mainTextColor model), paddingB 1]
    , mutedLabel model "Haskell workflow"
        `styleBasic` [paddingB 14]
    , vstack (map (sideNavButton model) (vmNavItems vm))
    , sidebarStatusBlock model vm
        `styleBasic` [paddingT 12]
    , sidebarLibraryStats model
        `styleBasic` [paddingT 12]
    , sidebarProgressBlock model
        `styleBasic` [paddingT 12]
    , sidebarFailureBlock model
        `styleBasic` [paddingT 12]
    , filler
    , secondaryButton model "Open data folder" GuiOpenDataFolder
    , secondaryButton model "Open logs" GuiOpenLogs
        `styleBasic` [paddingT 6]
    , secondaryButton model "Support bundle" GuiCreateSupportBundle
        `styleBasic` [paddingT 6]
    , secondaryButton model "Refresh" GuiRefresh
        `styleBasic` [paddingT 6]
    ]
    `styleBasic` [width 188, padding 10, bgColor (panelAltColor model), border 1 (borderColor model)]

sideNavButton :: Model -> NavItem -> WidgetNode Model GuiEvent
sideNavButton model item =
  button (navLabel item) (GuiNavigate (navView item))
    `styleBasic`
      [ paddingH 10
      , paddingV 2
      , height 28
      , width 166
      , radius 8
      , textSize 12
      , textColor (if navActive item then primaryTextColor model else mainTextColor model)
      , bgColor (if navActive item then primaryColor model else panelBgColor model)
      , border 1 (if navActive item then primaryColor model else borderColor model)
      , paddingB 6
      ]

sidebarStatusBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
sidebarStatusBlock model vm =
  vstack
    ( mutedLabel model "Connections"
        : map (statusLabel model) (vmStatusBadges vm)
    )
    `styleBasic` [padding 10, radius 12, bgColor (panelBgColor model), border 1 (borderColor model)]

sidebarLibraryStats :: Model -> WidgetNode Model GuiEvent
sidebarLibraryStats model =
  case libraryStats model of
    Nothing -> emptyBlock
    Just stats ->
      vstack
        [ mutedLabel model "Library"
        , sidebarStat model "Articles" (totalArticles stats)
        , sidebarStat model "Uploaded" (uploadedArticles stats)
        , sidebarStat model "Avg words" (averageWordCount stats)
        ]
        `styleBasic` [padding 10, radius 12, bgColor (panelBgColor model), border 1 (borderColor model)]

sidebarStat :: Model -> Text -> Int -> WidgetNode Model GuiEvent
sidebarStat model name value =
  hstack
    [ mutedLabel model name
    , filler
    , label (tshow value)
        `styleBasic` [textSize 13, textColor (mainTextColor model)]
    ]
    `styleBasic` [paddingT 4]

sidebarProgressBlock :: Model -> WidgetNode Model GuiEvent
sidebarProgressBlock model =
  case activeProgress model of
    Nothing -> emptyBlock
    Just progress ->
      vstack
        [ mutedLabel model "Current job"
        , label_ (progressLabel progress) [ellipsis]
            `styleBasic` [textSize 12, textColor (mainTextColor model), paddingT 4]
        , progressMeter model 158 progress
            `styleBasic` [paddingT 7]
        , label (progressCountText progress)
            `styleBasic` [textSize 11, textColor (mutedTextColor model), paddingT 5]
        , label_ (progressDetail progress) [ellipsis]
            `styleBasic` [textSize 10, textColor (mutedTextColor model), paddingT 3]
        ]
        `styleBasic` [padding 10, radius 12, bgColor (panelBgColor model), border 1 (borderColor model)]

sidebarFailureBlock :: Model -> WidgetNode Model GuiEvent
sidebarFailureBlock model
  | null (failedFetches model) && null (failedUploads model) = emptyBlock
  | otherwise =
      vstack
        [ mutedLabel model "Retry list"
        , failureCountLine model "Fetch" (length (failedFetches model))
        , failureCountLine model "Upload" (length (failedUploads model))
        , vstack (map (failureLine model) (take 3 (map fst (failedFetches model) <> map (tshow . fst) (failedUploads model))))
        , hstack
            [ rowSecondaryButton model "Retry fetch" GuiRetryFailedFetches
            , rowSecondaryButton model "Retry upload" GuiRetryFailedUploads
            ]
            `styleBasic` [paddingT 6]
        , rowDangerButton model "Clear" GuiClearFailures
            `styleBasic` [paddingT 4]
        ]
        `styleBasic` [padding 10, radius 12, bgColor (panelBgColor model), border 1 (borderColor model)]

failureCountLine :: Model -> Text -> Int -> WidgetNode Model GuiEvent
failureCountLine model name value =
  hstack
    [ mutedLabel model name
    , filler
    , label (tshow value)
        `styleBasic` [textSize 12, textColor (if value > 0 then dangerColor model else mutedTextColor model)]
    ]
    `styleBasic` [paddingT 4]

failureLine :: Model -> Text -> WidgetNode Model GuiEvent
failureLine model text =
  label_ text [ellipsis]
    `styleBasic` [textSize 10, textColor (mutedTextColor model), paddingT 3]

statusBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
statusBlock model vm =
  hstack (map (statusLabel model) (vmStatusBadges vm))

statusLabel :: Model -> StatusBadge -> WidgetNode Model GuiEvent
statusLabel model badge =
  label (statusName badge <> ": " <> statusText badge)
    `styleBasic`
      [ paddingH 10
      , paddingV 5
      , paddingR 12
      , radius 12
      , textSize 13
      , textColor (if statusConnected badge then primaryColor model else warningColor model)
      , bgColor (panelAltColor model)
      ]

notificationBlock :: Model -> WidgetNode Model GuiEvent
notificationBlock model =
  case notification model of
    Nothing -> emptyBlock
    Just notice ->
      hstack
        [ label (noticeText notice)
            `styleBasic` [textColor (noticeColor model notice)]
        , filler
        , secondaryButton model "Clear" GuiClearNotice
        ]
        `styleBasic` [padding 10, bgColor (panelBgColor model), radius 10, border 1 (borderColor model), paddingB 8]

progressNoticeBlock :: Model -> WidgetNode Model GuiEvent
progressNoticeBlock model =
  case activeProgress model of
    Nothing -> emptyBlock
    Just progress ->
      vstack
        [ hstack
            [ label (progressLabel progress)
                `styleBasic` [textColor (mainTextColor model)]
            , filler
            , label (progressCountText progress)
                `styleBasic` [textColor (mutedTextColor model), textSize 12]
            ]
        , progressMeter model 360 progress
            `styleBasic` [paddingT 7]
        , label_ (progressDetail progress) [ellipsis]
            `styleBasic` [textColor (mutedTextColor model), textSize 11, paddingT 5]
        ]
        `styleBasic` [padding 10, bgColor (panelBgColor model), radius 10, border 1 (borderColor model), paddingB 8]

progressMeter :: Model -> Double -> ProgressStatus -> WidgetNode Model GuiEvent
progressMeter model meterWidth progress =
  zstack
    [ spacer
        `styleBasic` [height 7, width meterWidth, radius 4, bgColor (borderColor model)]
    , spacer
        `styleBasic` [height 7, width fillWidth, radius 4, bgColor (primaryColor model)]
    ]
  where
    fillWidth =
      if progressTotal progress <= 0
        then 0
        else max 6 (meterWidth * progressFraction progress)

progressFraction :: ProgressStatus -> Double
progressFraction progress
  | progressTotal progress <= 0 = 0
  | otherwise =
      min 1 (fromIntegral (max 0 (progressCurrent progress)) / fromIntegral (progressTotal progress))

progressCountText :: ProgressStatus -> Text
progressCountText progress =
  tshow (progressCurrent progress) <> " / " <> tshow (progressTotal progress)

primaryButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
primaryButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 7
      , paddingV 2
      , height 26
      , radius 7
      , textSize 12
      , textColor (primaryTextColor model)
      , bgColor (primaryColor model)
      , border 1 (primaryColor model)
      ]

secondaryButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
secondaryButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 7
      , paddingV 2
      , height 26
      , radius 7
      , textSize 12
      , textColor (mainTextColor model)
      , bgColor (panelAltColor model)
      , border 1 (borderColor model)
      ]

dangerButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
dangerButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 7
      , paddingV 2
      , height 26
      , radius 7
      , textSize 12
      , textColor (mainTextColor model)
      , bgColor (dangerBgColor model)
      , border 1 (dangerColor model)
      ]

rowPrimaryButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
rowPrimaryButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor (primaryTextColor model)
      , bgColor (primaryColor model)
      , border 1 (primaryColor model)
      ]

rowSecondaryButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
rowSecondaryButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor (mainTextColor model)
      , bgColor (panelAltColor model)
      , border 1 (borderColor model)
      ]

rowDangerButton :: Model -> Text -> GuiEvent -> WidgetNode Model GuiEvent
rowDangerButton model caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor (mainTextColor model)
      , bgColor (dangerBgColor model)
      , border 1 (dangerColor model)
      ]

inputStyle :: Model -> [StyleState]
inputStyle model =
  [ paddingH 7
  , paddingV 2
  , height 26
  , radius 7
  , textSize 12
  , textColor (mainTextColor model)
  , bgColor (panelAltColor model)
  , border 1 (borderColor model)
  ]

panelStyle :: Model -> [StyleState]
panelStyle model =
  [ padding 8
  , radius 12
  , bgColor (panelBgColor model)
  , border 1 (borderColor model)
  ]

mutedLabel :: Model -> Text -> WidgetNode Model GuiEvent
mutedLabel model caption =
  label caption
    `styleBasic` [textSize 13, textColor (mutedTextColor model)]

emptyBlock :: WidgetNode Model GuiEvent
emptyBlock =
  spacer_ [width 0]

sectionLabelForId :: Text -> Text
sectionLabelForId ident =
  maybe ident sectionLabel (findSection ident)

findSection :: Text -> Maybe Section
findSection ident =
  go allSections
  where
    go [] = Nothing
    go (section : rest)
      | sectionId section == ident = Just section
      | otherwise = go rest

contentBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
contentBlock model vm =
  vstack
    [ screenSummary model vm
    , zeitControls model
    , diagnosticsControls model
    , browseControls model
    , libraryControls model
    , lingqControls model
    , selectedArticleBlock model (vmSelectedArticle vm)
    , articleParagraphsBlock model (vmSelectedArticleParagraphs vm)
    , articleRowsBlock model (rowsForCurrentView model)
    ]
    `styleBasic` (panelStyle model)

screenSummary :: Model -> AppViewModel -> WidgetNode Model GuiEvent
screenSummary model vm =
  hstack
    [ mutedLabel model ("Section: " <> vmBrowseSection vm)
    , mutedLabel model ("Filter: " <> vmActiveFilter vm)
        `styleBasic` [paddingL 16]
    , mutedLabel model (vmDatePrefix vm)
        `styleBasic` [paddingL 16]
    ]
    `styleBasic` [paddingB 8]

zeitControls :: Model -> WidgetNode Model GuiEvent
zeitControls model =
  case currentView model of
    ZeitLoginView ->
      vstack
        [ label "Zeit cookie session"
            `styleBasic` [textSize 16, paddingT 8]
        , label "Use Browser login & import first. It opens real Edge/Chrome, imports the zeit.de cookies, and reuses that browser user-agent for article requests."
            `styleBasic` [textSize 12, paddingB 6]
        , label_ ("Fetch identity: " <> compactUserAgent (zeitUserAgentText model)) [ellipsis]
            `styleBasic` [textSize 11, textColor (mutedTextColor model), paddingB 6]
        , textAreaV_
            (zeitCookieText model)
            GuiZeitCookieChanged
            [maxLines 5]
            `styleBasic` (inputStyle model <> [height 110])
        , hstack
            [ primaryButton model "Browser login & import" GuiZeitBrowserLogin
            , secondaryButton model "Open Zeit login page" GuiOpenZeitLoginPage
            , primaryButton model "Save cookie session" GuiZeitCookieLogin
            , secondaryButton model "Disconnect Zeit" GuiZeitLogout
            ]
            `styleBasic` [paddingT 6]
        ]
    _ -> emptyBlock

compactUserAgent :: Text -> Text
compactUserAgent raw
  | T.null stripped = "default browser-like request headers"
  | otherwise = stripped
  where
    stripped = T.strip raw

diagnosticsControls :: Model -> WidgetNode Model GuiEvent
diagnosticsControls model =
  case currentView model of
    DiagnosticsView ->
      vstack
        [ diagnosticsOverview model
        , diagnosticsJobPanel model
        , diagnosticsFailurePanel model
        , diagnosticsHistoryPanel model
        ]
        `styleBasic` [paddingB 8]
    _ -> emptyBlock

diagnosticsOverview :: Model -> WidgetNode Model GuiEvent
diagnosticsOverview model =
  vstack
    [ hstack
        [ diagnosticStat model "Queue" (tshow (length (queuedJobs model)))
        , diagnosticStat model "Completed" (tshow (length (completedJobs model)))
        , diagnosticStat model "Fetch failures" (tshow (length (failedFetches model)))
        , diagnosticStat model "Upload failures" (tshow (length (failedUploads model)))
        ]
    , hstack
        [ secondaryButton model "Open data folder" GuiOpenDataFolder
        , secondaryButton model "Open logs" GuiOpenLogs
        , secondaryButton model "Copy recent log" GuiCopyRecentLog
        , secondaryButton model "Support bundle" GuiCreateSupportBundle
        ]
        `styleBasic` [paddingT 8]
    ]
    `styleBasic` [padding 8, radius 12, bgColor (panelAltColor model), border 1 (borderColor model), paddingB 8]

diagnosticStat :: Model -> Text -> Text -> WidgetNode Model GuiEvent
diagnosticStat model caption value =
  vstack
    [ label value
        `styleBasic` [textSize 18, textColor (primaryColor model)]
    , label caption
        `styleBasic` [textSize 11, textColor (mutedTextColor model)]
    ]
    `styleBasic` [width 116, paddingR 10]

diagnosticsJobPanel :: Model -> WidgetNode Model GuiEvent
diagnosticsJobPanel model =
  vstack
    [ label "Jobs"
        `styleBasic` [textSize 15, textColor (mainTextColor model)]
    , currentJobLine model
        `styleBasic` [paddingT 6]
    , hstack
        [ label ("Queue: " <> (if jobQueuePaused model then "paused" else "running"))
            `styleBasic` [textColor (mutedTextColor model), paddingR 12]
        , secondaryButton model (if jobQueuePaused model then "Resume queue" else "Pause queue") (GuiQueuePausedChanged (not (jobQueuePaused model)))
        , secondaryButton model "Cancel current" GuiCancelCurrentJob
        , secondaryButton model "Run next" GuiRunNextQueuedJob
        , dangerButton model "Clear queue" GuiClearQueuedJobs
        ]
        `styleBasic` [paddingT 8]
    , queuedJobList model
        `styleBasic` [paddingT 8]
    ]
    `styleBasic` [padding 8, radius 12, bgColor (panelBgColor model), border 1 (borderColor model), paddingB 8]

currentJobLine :: Model -> WidgetNode Model GuiEvent
currentJobLine model =
  case activeProgress model of
    Nothing ->
      mutedLabel model "No running batch job."
    Just progress ->
      vstack
        [ hstack
            [ label (progressLabel progress)
                `styleBasic` [textColor (mainTextColor model)]
            , filler
            , mutedLabel model (progressCountText progress)
            ]
        , progressMeter model 420 progress
            `styleBasic` [paddingT 7]
        , label_ (progressDetail progress) [ellipsis]
            `styleBasic` [textColor (mutedTextColor model), textSize 11, paddingT 5]
        ]

queuedJobList :: Model -> WidgetNode Model GuiEvent
queuedJobList model
  | null (queuedJobs model) = mutedLabel model "No queued work."
  | otherwise =
      vstack (map (queuedJobLine model) (take 8 (queuedJobs model)))

queuedJobLine :: Model -> QueuedJob -> WidgetNode Model GuiEvent
queuedJobLine model job =
  hstack
    [ label (queuedJobLabel job)
        `styleBasic` [textColor (mainTextColor model), width 300]
    , mutedLabel model (jobKindLabel (queuedJobKind job))
    , filler
    , mutedLabel model (tshow (queuedJobItemCount job) <> " item(s)")
    ]
    `styleBasic` [paddingT 4]

diagnosticsFailurePanel :: Model -> WidgetNode Model GuiEvent
diagnosticsFailurePanel model =
  vstack
    [ label "Retry Lists"
        `styleBasic` [textSize 15, textColor (mainTextColor model)]
    , hstack
        [ failureCountLine model "Fetch failures" (length (failedFetches model))
        , failureCountLine model "Upload failures" (length (failedUploads model))
        , secondaryButton model "Retry fetches" GuiRetryFailedFetches
        , secondaryButton model "Retry uploads" GuiRetryFailedUploads
        , dangerButton model "Clear failures" GuiClearFailures
        ]
        `styleBasic` [paddingT 6]
    , vstack (map (failureLine model) (take 5 (map fst (failedFetches model) <> map (tshow . fst) (failedUploads model))))
        `styleBasic` [paddingT 4]
    ]
    `styleBasic` [padding 8, radius 12, bgColor (panelBgColor model), border 1 (borderColor model), paddingB 8]

diagnosticsHistoryPanel :: Model -> WidgetNode Model GuiEvent
diagnosticsHistoryPanel model =
  vstack
    [ hstack
        [ label "Completed Jobs"
            `styleBasic` [textSize 15, textColor (mainTextColor model)]
        , filler
        , dangerButton model "Clear history" GuiClearCompletedJobs
        ]
    , if null (completedJobs model)
        then mutedLabel model "No completed batch jobs yet."
        else vstack (map (completedJobLine model) (take 8 (completedJobs model)))
    ]
    `styleBasic` [padding 8, radius 12, bgColor (panelBgColor model), border 1 (borderColor model), paddingB 8]

completedJobLine :: Model -> CompletedJob -> WidgetNode Model GuiEvent
completedJobLine model job =
  vstack
    [ hstack
        [ label (completedJobLabel job)
            `styleBasic` [textColor (mainTextColor model), width 300]
        , mutedLabel model (jobKindLabel (completedJobKind job))
        , filler
        , label (if completedJobSucceeded job then "success" else "needs attention")
            `styleBasic` [textSize 11, textColor (if completedJobSucceeded job then primaryColor model else warningColor model)]
        ]
    , label_ (completedJobSummary job) [ellipsis]
        `styleBasic` [textSize 11, textColor (mutedTextColor model), paddingT 2]
    ]
    `styleBasic` [paddingT 6]

queuedJobKind :: QueuedJob -> JobKind
queuedJobKind QueuedFetchJob {} = FetchJob
queuedJobKind QueuedUploadJob {} = UploadJob

jobKindLabel :: JobKind -> Text
jobKindLabel FetchJob = "Fetch"
jobKindLabel UploadJob = "Upload"

browseControls :: Model -> WidgetNode Model GuiEvent
browseControls model =
  case currentView model of
    BrowseView ->
      vstack
        [ hstack
            [ label "Topic"
                `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
            , textDropdownV_
                (browseSectionId model)
                GuiSectionSelected
                (map sectionId allSections)
                sectionLabelForId
                [maxHeight 360]
                `styleBasic` (inputStyle model <> [width 240])
            , label "Search"
                `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV_ (browseSearch model) GuiBrowseSearchChanged [placeholder "title or topic"]
                `styleBasic` (inputStyle model <> [width 190])
            , secondaryButton model "Previous" GuiBrowsePreviousPage
            , label ("Page " <> tshow (browsePage model))
                `styleBasic` [paddingH 10, textColor (mainTextColor model)]
            , secondaryButton model "Next" GuiBrowseNextPage
            , label "Fetch min"
                `styleBasic` [paddingL 14, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV (maybe "" tshow (minWords (browseFilter model))) GuiBrowseMinWordsChanged
                `styleBasic` (inputStyle model <> [width 72])
            , label "max"
                `styleBasic` [paddingL 10, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV (maybe "" tshow (maxWords (browseFilter model))) GuiBrowseMaxWordsChanged
                `styleBasic` (inputStyle model <> [width 72])
            ]
            `styleBasic` [paddingB 8]
        , hstack
            [ toggle model "Only new" (browseOnlyNew model) GuiBrowseOnlyNewChanged
            , toggle model "Show hidden" (browseShowHidden model) GuiBrowseShowHiddenChanged
            , secondaryButton model "Select shown" (GuiBrowseSelectVisible visible)
            , secondaryButton model "Deselect" GuiBrowseClearSelection
            , label (tshow (Set.size (browseSelectedUrls model)) <> " selected")
                `styleBasic` [paddingL 8, paddingR 12, textColor (mutedTextColor model)]
            , primaryButton model ("Fetch selected (" <> tshow (Set.size (browseSelectedUrls model)) <> ")") (GuiFetchSelected visible)
            , secondaryButton model ("Fetch shown (" <> tshow (length visible) <> ")") (GuiFetchVisible visible)
            , mutedLabel model (tshow (length visible) <> " shown / " <> tshow (length (browseArticles model)) <> " loaded")
            ]
            `styleBasic` [paddingB 8]
        ]
        `styleBasic` [paddingB 8]
    _ -> emptyBlock
  where
    visible = visibleBrowseArticles model

libraryControls :: Model -> WidgetNode Model GuiEvent
libraryControls model =
  case currentView model of
    LibraryView ->
      vstack
        [ libraryStatsBlock model
        , hstack
            [ label "Preset"
                `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
            , textDropdownV_
                (libraryPreset model)
                GuiLibraryPresetChanged
                allLibraryPresets
                libraryPresetLabel
                []
                `styleBasic` (inputStyle model <> [width 160])
            , label "Search"
                `styleBasic` [paddingL 12, paddingR 8, textColor (mutedTextColor model)]
            , textFieldV_ (maybe "" id (librarySearch query)) GuiLibrarySearchChanged [placeholder "Title or article text"]
                `styleBasic` (inputStyle model <> [width 260])
            , label "Min"
                `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV (maybe "" tshow (minWords (libraryWordFilter query))) GuiLibraryMinWordsChanged
                `styleBasic` (inputStyle model <> [width 70])
            , label "Max"
                `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV (maybe "" tshow (maxWords (libraryWordFilter query))) GuiLibraryMaxWordsChanged
                `styleBasic` (inputStyle model <> [width 70])
            , label "Sort"
                `styleBasic` [paddingL 12, paddingR 6, textColor (mutedTextColor model)]
            , textDropdownV_
                (librarySort query)
                GuiLibrarySortChanged
                allLibrarySorts
                librarySortLabel
                []
                `styleBasic` (inputStyle model <> [width 130])
            ]
        , librarySectionControls model
        , hstack
            [ toggle model "Show ignored" (libraryIncludeIgnored query) GuiLibraryIncludeIgnoredChanged
            , toggle model "Only ignored" (libraryOnlyIgnored query) GuiLibraryOnlyIgnoredChanged
            , toggle model "Only not uploaded" (libraryOnlyNotUploaded query) GuiLibraryOnlyNotUploadedChanged
            , toggle model "Group by section" (libraryGroupBySection model) GuiLibraryGroupBySectionChanged
            ]
            `styleBasic` [paddingV 8]
        , hstack
            [ secondaryButton model "Previous" GuiLibraryPreviousPage
            , label (libraryPageLabel model)
                `styleBasic` [paddingH 12, textColor (mainTextColor model)]
            , secondaryButton model "Next" GuiLibraryNextPage
            ]
        , hstack
            [ dangerButton model "Delete ignored" GuiDeleteIgnoredArticles
            , label "Older than"
                `styleBasic` [paddingL 10, paddingR 6, textColor (mutedTextColor model)]
            , textFieldV (libraryDeleteDaysText model) GuiLibraryDeleteDaysChanged
                `styleBasic` (inputStyle model <> [width 56])
            , mutedLabel model "days"
            , dangerButton model "Delete old" (GuiDeleteOldArticles False False)
            , dangerButton model "Delete uploaded" (GuiDeleteOldArticles True False)
            , dangerButton model "Delete unuploaded" (GuiDeleteOldArticles False True)
            ]
            `styleBasic` [paddingT 8]
        ]
        `styleBasic` [paddingV 8]
    _ -> emptyBlock
  where
    query = libraryQuery model

libraryStatsBlock :: Model -> WidgetNode Model GuiEvent
libraryStatsBlock model =
  case libraryStats model of
    Nothing -> emptyBlock
    Just stats ->
      hstack
        [ statLabel model "Articles" (totalArticles stats)
        , statLabel model "Uploaded" (uploadedArticles stats)
        , statLabel model "Avg words" (averageWordCount stats)
        ]
        `styleBasic` [paddingB 8]

statLabel :: Model -> Text -> Int -> WidgetNode Model GuiEvent
statLabel model name value =
  vstack
    [ label (tshow value)
        `styleBasic` [textSize 18, textColor (primaryColor model)]
    , label name
        `styleBasic` [textSize 11, textColor (mutedTextColor model)]
    ]
    `styleBasic` [paddingR 24]

librarySectionControls :: Model -> WidgetNode Model GuiEvent
librarySectionControls model =
  case libraryStats model of
    Nothing -> emptyBlock
    Just _stats ->
      hstack
        [ label "Section"
            `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
        , textDropdownV_
            activeSectionValue
            GuiLibrarySectionChanged
            ("" : map fst sections)
            sectionText
            [maxHeight 320]
            `styleBasic` (inputStyle model <> [width 300])
        ]
        `styleBasic` [paddingT 8, paddingB 4]
  where
    activeSection = librarySection (libraryQuery model)
    sections = maybe [] (Map.toList . sectionCounts) (libraryStats model)
    activeSectionValue = maybe "" id activeSection
    sectionText "" = "All sections"
    sectionText sectionName =
      sectionName <> maybe "" (\total -> " (" <> tshow total <> ")") (Map.lookup sectionName (Map.fromList sections))

allLibrarySorts :: [LibrarySort]
allLibrarySorts =
  [ LibrarySortNewest
  , LibrarySortOldest
  , LibrarySortLongest
  , LibrarySortShortest
  , LibrarySortTitle
  ]

librarySortLabel :: LibrarySort -> Text
librarySortLabel sortMode =
  case sortMode of
    LibrarySortNewest -> "Newest"
    LibrarySortOldest -> "Oldest"
    LibrarySortLongest -> "Longest"
    LibrarySortShortest -> "Shortest"
    LibrarySortTitle -> "Title"

allLibraryPresets :: [LibraryPreset]
allLibraryPresets =
  [ LibraryPresetAll
  , LibraryPresetShortReads
  , LibraryPresetStandardReads
  , LibraryPresetLongReads
  , LibraryPresetNotUploaded
  , LibraryPresetDuplicateReview
  , LibraryPresetCustom
  ]

libraryPresetLabel :: LibraryPreset -> Text
libraryPresetLabel preset =
  case preset of
    LibraryPresetAll -> "All articles"
    LibraryPresetShortReads -> "Short LingQ reads"
    LibraryPresetStandardReads -> "Standard LingQ reads"
    LibraryPresetLongReads -> "Long reads"
    LibraryPresetNotUploaded -> "Not uploaded"
    LibraryPresetDuplicateReview -> "Duplicate review"
    LibraryPresetCustom -> "Custom"

allRowDensities :: [RowDensity]
allRowDensities = [CompactRows, ComfortableRows]

rowDensityLabel :: RowDensity -> Text
rowDensityLabel CompactRows = "Compact"
rowDensityLabel ComfortableRows = "Comfort"

allUiThemes :: [UiTheme]
allUiThemes = [DarkUiTheme, LightUiTheme]

uiThemeLabel :: UiTheme -> Text
uiThemeLabel DarkUiTheme = "Dark"
uiThemeLabel LightUiTheme = "Light"

toggle :: Model -> Text -> Bool -> (Bool -> GuiEvent) -> WidgetNode Model GuiEvent
toggle model text value handler =
  hstack
    [ checkboxV value handler
    , label text
        `styleBasic` [paddingL 5, paddingR 14, textColor (mainTextColor model)]
    ]

libraryPageLabel :: Model -> Text
libraryPageLabel model
  | libraryTotal model <= 0 = "0 of 0"
  | otherwise =
      tshow firstItem
        <> "-"
        <> tshow lastItem
        <> " of "
        <> tshow (libraryTotal model)
  where
    query = libraryQuery model
    firstItem = libraryOffset query + 1
    lastItem = min (libraryTotal model) (libraryOffset query + libraryLimit query)

lingqControls :: Model -> WidgetNode Model GuiEvent
lingqControls model =
  case currentView model of
    LingqView ->
      vstack
        [ lingqLoginControls model
        , lingqFilterControls model
        , lingqTargetControls model
        , hstack
            [ secondaryButton model ("Select not uploaded (" <> tshow (length uploadable) <> ")") (GuiLingqSelectNotUploaded (lingqArticles model))
            , secondaryButton model "Deselect" GuiLingqClearSelection
            , label (tshow (Set.size (lingqSelectedIds model)) <> " selected")
                `styleBasic` [paddingH 8, textColor (mutedTextColor model)]
            , primaryButton model ("Upload selected (" <> tshow (Set.size (lingqSelectedIds model)) <> ")") (GuiUploadSelected (lingqArticles model))
            , secondaryButton model ("Upload visible (" <> T.pack (show (length uploadable)) <> ")") (GuiUploadVisible (lingqArticles model))
            , secondaryButton model "Sync status" GuiSyncLingqStatus
            ]
            `styleBasic` [paddingB 6]
        , hstack
            [ secondaryButton model "Sync known words" GuiSyncKnownWords
            , secondaryButton model "Refresh %" GuiComputeKnownWords
            , secondaryButton model "Refresh collections" GuiRefreshCollections
            , toggle model "Auto-date lesson titles" (datePrefixEnabled model) GuiDatePrefixChanged
            ]
        , label ("Known stems (" <> lingqLanguage model <> "): " <> tshow (knownStemTotal model))
            `styleBasic` [paddingT 8, textColor (mutedTextColor model)]
        , hstack
            [ secondaryButton model
                (if lingqShowKnownImport model then "Hide known-word import" else "Import known words")
                (GuiLingqKnownImportVisible (not (lingqShowKnownImport model)))
            , dangerButton model "Clear known words" GuiClearKnownWords
            , secondaryButton model
                (if lingqShowSectionMappings model then "Hide section mapping" else "Map sections")
                (GuiLingqMappingsVisible (not (lingqShowSectionMappings model)))
            ]
            `styleBasic` [paddingT 6]
        , knownImportPanel model
        , sectionMappingsPanel model
        ]
        `styleBasic` [paddingV 8]
    _ -> emptyBlock
  where
    uploadable =
      [ article
      | article <- lingqArticles model
      , not (summaryUploaded article)
      , not (summaryIgnored article)
      ]

lingqFilterControls :: Model -> WidgetNode Model GuiEvent
lingqFilterControls model =
  hstack
    [ toggle model "Only not uploaded" (lingqOnlyNotUploaded model) GuiLingqOnlyNotUploadedChanged
    , label "Min"
        `styleBasic` [paddingL 10, paddingR 6, textColor (mutedTextColor model)]
    , textFieldV (maybe "" tshow (minWords (lingqFilter model))) GuiLingqMinWordsChanged
        `styleBasic` (inputStyle model <> [width 70])
    , label "Max"
        `styleBasic` [paddingL 10, paddingR 6, textColor (mutedTextColor model)]
    , textFieldV (maybe "" tshow (maxWords (lingqFilter model))) GuiLingqMaxWordsChanged
        `styleBasic` (inputStyle model <> [width 70])
    , mutedLabel model (tshow (length (lingqArticles model)) <> " upload candidates")
    ]
    `styleBasic` [paddingB 8]

lingqTargetControls :: Model -> WidgetNode Model GuiEvent
lingqTargetControls model =
  hstack
    [ label "Language"
        `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
    , textDropdownV_
        (lingqLanguage model)
        GuiLingqLanguageChanged
        (languageOptions model)
        (languageLabelFor model)
        [maxHeight 320]
        `styleBasic` (inputStyle model <> [width 210])
    , secondaryButton model "Refresh languages" GuiRefreshLanguages
    , label "Course"
        `styleBasic` [paddingL 16, paddingR 8, textColor (mutedTextColor model)]
    , collectionPicker model
    , mutedLabel model (if null (lingqCollections model) then "Refresh collections to use names." else "Blank keeps lessons standalone.")
    ]
    `styleBasic` [paddingB 8]

collectionPicker :: Model -> WidgetNode Model GuiEvent
collectionPicker model =
  if null (lingqCollections model)
    then textFieldV_
          (maybe "" id (lingqFallbackCollection model))
          GuiFallbackCollectionChanged
          [placeholder "collection id or blank"]
          `styleBasic` (inputStyle model <> [width 230])
    else textDropdownV_
          (maybe "" id (lingqFallbackCollection model))
          GuiFallbackCollectionChanged
          ("" : map collectionId (lingqCollections model))
          (collectionLabelFor model)
          [maxHeight 320]
          `styleBasic` (inputStyle model <> [width 320])

languageOptions :: Model -> [Text]
languageOptions model =
  uniqueTexts (lingqLanguage model : map languageCode (lingqLanguages model) <> commonLingqLanguages)

commonLingqLanguages :: [Text]
commonLingqLanguages =
  ["de", "en", "es", "fr", "it", "pt", "nl", "sv", "pl", "ru", "ja", "zh"]

languageLabelFor :: Model -> Text -> Text
languageLabelFor model code =
  case filter ((== code) . languageCode) (lingqLanguages model) of
    language : _ -> languageTitle language <> " (" <> languageCode language <> ")"
    [] -> code

uniqueTexts :: [Text] -> [Text]
uniqueTexts =
  reverse . snd . foldl addUnique (Set.empty, [])
  where
    addUnique (seen, values) value
      | T.null value || Set.member value seen = (seen, values)
      | otherwise = (Set.insert value seen, value : values)

collectionLabelFor :: Model -> Text -> Text
collectionLabelFor _ "" = "Standalone lesson"
collectionLabelFor model ident =
  case filter ((== ident) . collectionId) (lingqCollections model) of
    collection : _ -> collectionTitle collection <> " (" <> tshow (collectionLessonsCount collection) <> ")"
    [] -> ident

knownImportPanel :: Model -> WidgetNode Model GuiEvent
knownImportPanel model
  | not (lingqShowKnownImport model) = emptyBlock
  | otherwise =
      vstack
        [ label "Paste known words"
            `styleBasic` [paddingT 8, textColor (mainTextColor model)]
        , textAreaV_
            (knownImportText model)
            GuiKnownImportTextChanged
            [maxLines 6]
            `styleBasic` (inputStyle model <> [height 110])
        , primaryButton model "Import pasted words" GuiImportKnownWords
            `styleBasic` [paddingT 6]
        ]

sectionMappingsPanel :: Model -> WidgetNode Model GuiEvent
sectionMappingsPanel model
  | not (lingqShowSectionMappings model) = emptyBlock
  | otherwise =
      vscroll (vstack ((label "Per-section LingQ collections" `styleBasic` [textColor (mainTextColor model), paddingB 6]) : map collectionRow allSections))
        `styleBasic` [height 260, paddingT 8]
  where
    collectionRow section =
      hstack
        [ label (sectionLabel section)
            `styleBasic` [width 150, textColor (mutedTextColor model)]
        , textDropdownV_
            (Map.findWithDefault "" (sectionLabel section) (sectionCollections model))
            (GuiSectionCollectionChanged (sectionLabel section))
            ("" : map collectionId (lingqCollections model))
            (collectionLabelFor model)
            [maxHeight 260]
            `styleBasic` (inputStyle model <> [width 320])
        ]
        `styleBasic` [paddingB 4]

lingqLoginControls :: Model -> WidgetNode Model GuiEvent
lingqLoginControls model =
  vstack
    [ label "LingQ connection"
        `styleBasic` [textSize 16, paddingT 8, textColor (mainTextColor model)]
    , hstack
        [ label "API key"
            `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
        , textFieldV_
            (lingqApiKeyText model)
            GuiLingqApiKeyChanged
            [placeholder "LingQ API key"]
            `styleBasic` (inputStyle model <> [width 360])
        , primaryButton model "Connect API key" GuiLingqApiKeyLogin
        , secondaryButton model "Disconnect" GuiLingqLogout
        ]
    , hstack
        [ label "Username"
            `styleBasic` [paddingR 8, textColor (mutedTextColor model)]
        , textFieldV_
            (lingqUsernameText model)
            GuiLingqUsernameChanged
            [placeholder "LingQ username or email"]
            `styleBasic` (inputStyle model <> [width 230])
        , label "Password"
            `styleBasic` [paddingL 12, paddingR 8, textColor (mutedTextColor model)]
        , textFieldV_
            (lingqPasswordText model)
            GuiLingqPasswordChanged
            [placeholder "Password", textFieldDisplayChar '*']
            `styleBasic` (inputStyle model <> [width 220])
        , primaryButton model "Login with password" GuiLingqPasswordLogin
        ]
        `styleBasic` [paddingT 6]
    ]
    `styleBasic` [paddingB 10]

selectedArticleBlock :: Model -> Maybe ArticleRowView -> WidgetNode Model GuiEvent
selectedArticleBlock model Nothing =
  case currentView model of
    ArticleView -> label "No article selected."
    _ -> emptyBlock
selectedArticleBlock model (Just row) =
  vstack
    [ label (rowTitle row)
        `styleBasic` [textSize 18, paddingT 8, textColor (mainTextColor model)]
    , label (rowMeta row)
        `styleBasic` [textColor (mutedTextColor model)]
    , label (rowKnownPct row <> " | " <> rowUploadStatus row)
        `styleBasic` [textColor (mutedTextColor model)]
    , hstack (articleButtons model (selectedArticle model) (selectedArticleContent model))
        `styleBasic` [paddingT 6]
    ]

articleButtons :: Model -> Maybe ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
articleButtons model Nothing _ = [secondaryButton model "Back to library" GuiCloseArticle]
articleButtons model (Just article) maybeContent =
  [ secondaryButton model "Back to library" GuiCloseArticle
  , secondaryButton model "Original" (GuiOpenExternal (summaryUrl article))
  ]
    <> maybe [] (\content -> [secondaryButton model "Copy text" (GuiCopyText (articleCopyText content))]) maybeContent
    <> maybe [] (\ident -> uploadAction model ident article) (summaryId article)
    <> audioButtons model article maybeContent
    <> maybe [] (\ident -> [dangerButton model "Delete" (GuiDeleteArticle ident)]) (summaryId article)

audioButtons :: Model -> ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
audioButtons model summary maybeContent =
  case summaryId summary of
    Nothing -> []
    Just ident ->
      let maybeAudioUrl = maybeContent >>= articleAudioUrl
          maybeAudioPath = maybeContent >>= articleAudioPath
       in maybe [] (const [secondaryButton model "Download audio" (GuiDownloadAudio ident)]) maybeAudioUrl
            <> maybe [] (const [secondaryButton model "Open audio" (GuiOpenAudio ident)]) maybeAudioPath

articleParagraphsBlock :: Model -> [Text] -> WidgetNode Model GuiEvent
articleParagraphsBlock _ [] =
  emptyBlock
articleParagraphsBlock model paragraphs =
  vscroll (vstack (map (paragraphLabel model) paragraphs))
    `styleBasic` [height 280, paddingT 8, paddingB 8]

paragraphLabel :: Model -> Text -> WidgetNode Model GuiEvent
paragraphLabel model paragraph =
  label paragraph
    `styleBasic` [paddingB 8, textColor (mainTextColor model)]

articleRowsBlock :: Model -> [ArticleSummary] -> WidgetNode Model GuiEvent
articleRowsBlock model [] =
  label "No rows loaded yet."
    `styleBasic` [textColor (mutedTextColor model), paddingT 16]
articleRowsBlock model rows =
  vscroll (vstack rowWidgets)
    `styleBasic` [paddingT 6]
  where
    rowWidgets =
      case currentView model of
        LibraryView | libraryGroupBySection model -> concatMap groupedRows (groupArticlesBySection rows)
        _ -> map (articleRowBlock model) rows
    groupedRows (sectionName, articles) =
      ( label (sectionName <> " (" <> tshow (length articles) <> ")")
          `styleBasic` [textSize 15, textColor (primaryColor model), paddingT 8, paddingB 4]
      )
        : map (articleRowBlock model) articles

groupArticlesBySection :: [ArticleSummary] -> [(Text, [ArticleSummary])]
groupArticlesBySection =
  Map.toList . foldr addArticle Map.empty
  where
    addArticle article =
      Map.insertWith (<>) (sectionKey article) [article]
    sectionKey article
      | T.null (summarySection article) = "(uncategorized)"
      | otherwise = summarySection article

articleRowBlock :: Model -> ArticleSummary -> WidgetNode Model GuiEvent
articleRowBlock model article =
  hstack
    [ rowSelectionCheckbox model article
    , vstack
        [ label_ (rowTitle row) [ellipsis]
            `styleBasic` [textSize (rowTitleSize model), textColor (mainTextColor model)]
        , label_ (rowMeta row <> " | " <> rowKnownPct row <> " | " <> rowUploadStatus row) [ellipsis]
            `styleBasic` [textSize (rowMetaSize model), textColor (mutedTextColor model)]
        , hstack (rowActions model (currentView model) article)
            `styleBasic` [paddingT (rowActionPadding model)]
        ]
    ]
    `styleBasic`
      [ height (rowHeight model)
      , padding (rowPadding model)
      , radius 8
      , bgColor (panelAltColor model)
      , border 1 (borderColor model)
      ]
  where
    row = articleRowView article

rowHeight :: Model -> Double
rowHeight model =
  case rowDensity model of
    CompactRows -> 46
    ComfortableRows -> 58

rowPadding :: Model -> Double
rowPadding model =
  case rowDensity model of
    CompactRows -> 4
    ComfortableRows -> 6

rowTitleSize :: Model -> Double
rowTitleSize model =
  case rowDensity model of
    CompactRows -> 12
    ComfortableRows -> 13

rowMetaSize :: Model -> Double
rowMetaSize model =
  case rowDensity model of
    CompactRows -> 9
    ComfortableRows -> 10

rowActionPadding :: Model -> Double
rowActionPadding model =
  case rowDensity model of
    CompactRows -> 1
    ComfortableRows -> 3

rowSelectionCheckbox :: Model -> ArticleSummary -> WidgetNode Model GuiEvent
rowSelectionCheckbox model article =
  case currentView model of
    BrowseView ->
      checkboxV
        (Set.member (summaryUrl article) (browseSelectedUrls model))
        (const (GuiBrowseToggleSelection (summaryUrl article)))
        `styleBasic` [paddingR 8]
    LingqView ->
      case summaryId article of
        Nothing -> emptyBlock
        Just ident ->
          checkboxV
            (Set.member ident (lingqSelectedIds model))
            (const (GuiLingqToggleSelection ident))
            `styleBasic` [paddingR 8]
    _ -> emptyBlock

rowActions :: Model -> View -> ArticleSummary -> [WidgetNode Model GuiEvent]
rowActions model BrowseView article =
  [ rowSecondaryButton model "Preview" (GuiPreviewArticle article)
  , rowPrimaryButton model "Fetch" (GuiFetchArticle article)
  , rowSecondaryButton model "Original" (GuiOpenExternal (summaryUrl article))
  , rowSecondaryButton model
      (if summaryIgnored article then "Unhide" else "Hide")
      (if summaryIgnored article then GuiUnhideBrowseArticle article else GuiHideBrowseArticle article)
  ]
    <> maybe [] (const [rowSecondaryButton model "Open saved" (GuiOpenArticle article)]) (summaryId article)
rowActions model _ article =
  maybe
    []
    (\ident ->
      [ rowSecondaryButton model "Open" (GuiOpenArticle article)
      , rowSecondaryButton model "Original" (GuiOpenExternal (summaryUrl article))
      ]
        <> uploadAction model ident article
        <> [ rowSecondaryButton model
               (if summaryIgnored article then "Unignore" else "Ignore")
               (GuiToggleIgnored article)
           , rowDangerButton model "Delete" (GuiDeleteArticle ident)
           ])
    (summaryId article)

uploadAction :: Model -> ArticleId -> ArticleSummary -> [WidgetNode Model GuiEvent]
uploadAction model ident article
  | summaryIgnored article = []
  | summaryUploaded article = [rowSecondaryButton model "Update LingQ" (GuiUploadArticle ident)]
  | otherwise = [rowPrimaryButton model "Upload" (GuiUploadArticle ident)]

rowsForCurrentView :: Model -> [ArticleSummary]
rowsForCurrentView model =
  case currentView model of
    BrowseView -> visibleBrowseArticles model
    LibraryView -> libraryArticles model
    LingqView -> lingqArticles model
    ZeitLoginView -> []
    DiagnosticsView -> []
    ArticleView -> maybe [] (: []) (selectedArticle model)

visibleBrowseArticles :: Model -> [ArticleSummary]
visibleBrowseArticles model =
  filter matchesSearch (filter matchesOnlyNew (browseArticles model))
  where
    rawSearch = T.strip (T.toLower (browseSearch model))
    matchesOnlyNew article =
      not (browseOnlyNew model) || summaryId article == Nothing
    matchesSearch article
      | T.null rawSearch = True
      | otherwise =
          any
            (T.isInfixOf rawSearch . T.toLower)
            [ summaryTitle article
            , summarySection article
            , summaryUrl article
            ]

noticeText :: Notification -> Text
noticeText notice =
  noticeLevelText (notificationLevel notice) <> ": " <> notificationMessage notice

noticeLevelText :: NotificationLevel -> Text
noticeLevelText InfoNotice = "Info"
noticeLevelText SuccessNotice = "Success"
noticeLevelText ErrorNotice = "Error"

noticeColor :: Model -> Notification -> Color
noticeColor model notice =
  case notificationLevel notice of
    InfoNotice -> primaryColor model
    SuccessNotice -> primaryColor model
    ErrorNotice -> dangerColor model

loadGuiInitialModel :: AppPorts IO -> IO GuiEvent
loadGuiInitialModel ports =
  safeGuiTask $ do
    logInfo "App starting."
    model <- loadInitialModel (settingsPort ports)
    zeit <- loginToZeit (zeitPort ports)
    lingq <- loginToLingq (lingqPort ports) "" ""
    dispatchEvents ports model [ZeitStatusChanged zeit, LingqStatusChanged lingq, RefreshCurrentView]

runAppEvent :: AppPorts IO -> Model -> Event -> IO GuiEvent
runAppEvent ports model event =
  safeGuiTask (dispatchEvent ports model event)

runFetchBatchProducer :: GuiRuntime -> Model -> QueuedJob -> (GuiEvent -> IO ()) -> IO ()
runFetchBatchProducer runtime model job send =
  safeGuiProducer send $ do
    writeIORef cancelFlag False
    (results, cancelled) <- fetchMany [] (zip [1 ..] articles)
    sendProgress (length articles) "Updating the local library..."
    let failures = guiBatchFetchFailures results
        summary = withCancelSuffix cancelled (guiBatchFetchSummary results)
    finalModel <-
      dispatchEvents
        ports
        model
        [ BatchFetchFinished failures
        , CompletedJobRecorded (CompletedJob (queuedJobId job) FetchJob (queuedJobLabel job) summary (not cancelled && null failures))
        , Notify (if cancelled then InfoNotice else guiBatchFetchLevel results) summary
        , RefreshCurrentView
        ]
    send (GuiModelLoaded finalModel)
  where
    ports = guiPorts runtime
    cancelFlag = guiCancelFlag runtime
    articles = queuedFetchArticles job
    total = length articles
    sendProgress current detail =
      send (GuiProgress (Just (ProgressStatus (queuedJobLabel job) current total detail)))
    fetchMany acc [] = pure (reverse acc, False)
    fetchMany acc (item : rest) = do
      cancelled <- readIORef cancelFlag
      if cancelled
        then pure (reverse acc, True)
        else do
          result <- fetchOne item
          fetchMany (result : acc) rest
    fetchOne (index, article) = do
      sendProgress (index - 1) (summaryTitle article)
      result <- fetchArticleForBatch article
      sendProgress index (batchFetchProgressDetail result)
      pure result
    fetchArticleForBatch article = do
      let url = summaryUrl article
      fetched <- tryText (fetchArticleContent (zeitPort ports) url)
      case fetched of
        Left err -> pure (BatchFailed url err)
        Right fetchedArticle ->
          case applyWordFilter filters fetchedArticle of
            KeepArticle -> do
              saved <- tryText (saveArticle (libraryPort ports) fetchedArticle)
              pure $
                case saved of
                  Left err -> BatchFailed url err
                  Right savedId -> BatchSaved url savedId
            decision -> pure (BatchSkipped url decision)
    filters = queuedFetchFilter job

runUploadBatchProducer :: GuiRuntime -> Model -> QueuedJob -> (GuiEvent -> IO ()) -> IO ()
runUploadBatchProducer runtime model job send =
  safeGuiProducer send $ do
    writeIORef cancelFlag False
    now <- getCurrentTime
    envFallback <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
    let fallbackCollection = lingqFallbackCollection model <|> envFallback
        uploadIds = mapMaybeSummaryId (filter uploadableSummary articles)
        config =
          uploadConfigFromPreferences
            (utctDay now)
            (lingqLanguage model)
            fallbackCollection
            (datePrefixEnabled model)
            (sectionCollections model)
    if null uploadIds
      then do
        finalModel <-
          dispatchEvents
            ports
            model
            [ CompletedJobRecorded (CompletedJob (queuedJobId job) UploadJob (queuedJobLabel job) "No uploadable articles selected." False)
            , Notify ErrorNotice "No uploadable articles selected."
            ]
        send (GuiModelLoaded finalModel)
      else do
        sendProgress (ProgressStatus (queuedJobLabel job) 0 (length uploadIds) "Preparing selected articles...")
        loaded <- traverse loadOne uploadIds
        let loadFailures = [failure | Left failure <- loaded]
            uploadArticles = [article | Right article <- loaded]
        sendProgress (ProgressStatus (queuedJobLabel job) 0 (length uploadArticles) "Uploading to LingQ...")
        (uploadResults, cancelled) <- uploadMany config (length uploadArticles) [] (zip [1 ..] uploadArticles)
        let failures = loadFailures <> guiBatchUploadFailures uploadResults
            summary = withCancelSuffix cancelled (guiBatchUploadSummary loadFailures uploadResults)
        finalModel <-
          dispatchEvents
            ports
            model
            ( BatchUploadFinished failures
                : CompletedJobRecorded (CompletedJob (queuedJobId job) UploadJob (queuedJobLabel job) summary (not cancelled && null failures))
                : guiBatchUploadResultEvents cancelled loadFailures uploadResults
                  <> [RefreshCurrentView]
            )
        send (GuiModelLoaded finalModel)
  where
    ports = guiPorts runtime
    cancelFlag = guiCancelFlag runtime
    articles = queuedUploadArticles job
    sendProgress progress =
      send (GuiProgress (Just progress))
    uploadMany _config _total acc [] = pure (reverse acc, False)
    uploadMany config total acc (item : rest) = do
      cancelled <- readIORef cancelFlag
      if cancelled
        then pure (reverse acc, True)
        else do
          result <- uploadOne config total item
          uploadMany config total (result : acc) rest
    loadOne ident = do
      loaded <- tryText (loadArticle (libraryPort ports) ident)
      pure $
        case loaded of
          Left err -> Left (ident, err)
          Right Nothing -> Left (ident, "Article not found.")
          Right (Just article) -> Right article
    uploadOne config total (index, article) = do
      let titledArticle =
            article
              { articleTitle =
                  lessonTitle
                    (uploadDay config)
                    (uploadDatePrefixEnabled config)
                    (articleTitle article)
              }
          targetCollection = targetCollectionFor config article
          title = articleTitle titledArticle
      sendProgress (ProgressStatus (queuedJobLabel job) (index - 1) total title)
      result <-
        tryText $
          case articleUploadedLesson article of
            Just existingLesson ->
              updateLessonOnLingq (lingqPort ports) (uploadLanguageCode config) existingLesson titledArticle
            Nothing ->
              uploadLessonToLingq (lingqPort ports) (uploadLanguageCode config) targetCollection titledArticle
      uploadResult <-
        case result of
          Left err -> pure (UploadFailed (articleId article) title err)
          Right lesson ->
            case articleId article of
              Nothing -> pure (UploadSucceededUntracked title lesson)
              Just ident -> do
                markResult <- tryText (markArticleUploaded (libraryPort ports) ident lesson)
                pure $
                  case markResult of
                    Left err -> UploadFailed (Just ident) title ("Uploaded to LingQ, but local status update failed: " <> err)
                    Right () -> UploadSucceeded ident title lesson
      sendProgress (ProgressStatus (queuedJobLabel job) index total (batchUploadProgressDetail uploadResult))
      pure uploadResult

runUploadAppEvent :: AppPorts IO -> Model -> ArticleId -> IO GuiEvent
runUploadAppEvent ports model ident =
  safeGuiTask $ do
    now <- getCurrentTime
    envFallback <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
    let fallbackCollection = lingqFallbackCollection model <|> envFallback
    dispatchEvent ports model (ArticleUploadRequested (utctDay now) fallbackCollection ident)

runZeitBrowserLogin :: AppPorts IO -> Model -> IO GuiEvent
runZeitBrowserLogin ports model =
  safeGuiTask $ do
    session <- importZeitSessionViaBrowser
    dispatchEvent
      ports
      model
      (ZeitBrowserSessionLoginRequested (browserCookieHeader session) (browserUserAgent session))

importZeitSessionViaBrowser :: IO BrowserZeitSession
importZeitSessionViaBrowser
  | os /= "mingw32" =
      fail "Browser-assisted Zeit login is currently implemented for Windows Edge/Chrome only. Paste a Cookie header instead."
  | otherwise = do
      currentDir <- getCurrentDirectory
      let script = currentDir </> "scripts" </> "zeit-browser-login.ps1"
      exists <- doesFileExist script
      if not exists
        then fail ("Missing Zeit browser login helper: " <> script)
        else do
          (exitCode, out, err) <-
            readCreateProcessWithExitCode
              (proc "powershell.exe" ["-NoProfile", "-ExecutionPolicy", "Bypass", "-File", script])
              ""
          let session = parseBrowserZeitSession (T.strip (T.pack out))
              details = T.strip (T.pack err)
          case exitCode of
            ExitSuccess
              | not (T.null (browserCookieHeader session)) -> pure session
              | otherwise -> fail "Browser login finished without returning Zeit cookies."
            ExitFailure code ->
              fail (T.unpack ("Browser-assisted Zeit login failed (" <> tshow code <> "): " <> details))

parseBrowserZeitSession :: Text -> BrowserZeitSession
parseBrowserZeitSession raw =
  case eitherDecodeStrict' (encodeUtf8 raw) of
    Right session -> normalizeBrowserZeitSession session
    Left _ ->
      BrowserZeitSession
        { browserCookieHeader = T.strip raw
        , browserUserAgent = defaultZeitUserAgent
        }

normalizeBrowserZeitSession :: BrowserZeitSession -> BrowserZeitSession
normalizeBrowserZeitSession session =
  session
    { browserCookieHeader = T.strip (browserCookieHeader session)
    , browserUserAgent = normalizeZeitUserAgent (browserUserAgent session)
    }

runDeleteOlderAppEvent :: AppPorts IO -> Model -> Int -> Bool -> Bool -> IO GuiEvent
runDeleteOlderAppEvent ports model days onlyUploaded onlyUnuploaded =
  safeGuiTask $ do
    now <- getCurrentTime
    let cutoff = addUTCTime (negate (fromIntegral days * 86400)) now
    dispatchEvent ports model (LibraryDeleteOlderRequested cutoff onlyUploaded onlyUnuploaded)

safeGuiTask :: IO Model -> IO GuiEvent
safeGuiTask action = do
  result <- try action
  case result of
    Right model -> pure (GuiModelLoaded model)
    Left err -> do
      let message = T.pack (displayException (err :: SomeException))
      logError ("GUI task failed: " <> message)
      pure (GuiFailed message)

safeGuiProducer :: (GuiEvent -> IO ()) -> IO () -> IO ()
safeGuiProducer send action = do
  result <- try action
  case result of
    Right () -> pure ()
    Left err -> do
      logError ("Background job failed: " <> T.pack (displayException (err :: SomeException)))
      send (GuiProgress Nothing)
      send (GuiFailed (T.pack (displayException (err :: SomeException))))

ensureLogFile :: IO ()
ensureLogFile = do
  createDirectoryIfMissing True (takeDirectory logPath)
  TIO.appendFile logPath ""

logInfo :: Text -> IO ()
logInfo = logLine "INFO"

logError :: Text -> IO ()
logError = logLine "ERROR"

logLine :: Text -> Text -> IO ()
logLine level message = do
  createDirectoryIfMissing True (takeDirectory logPath)
  now <- getCurrentTime
  TIO.appendFile logPath ("[" <> tshow now <> "] [" <> level <> "] " <> message <> "\n")

tryText :: IO a -> IO (Either Text a)
tryText action = do
  result <- try action
  pure $
    case result of
      Right value -> Right value
      Left err -> Left (cleanExceptionText (T.pack (displayException (err :: SomeException))))

cleanExceptionText :: Text -> Text
cleanExceptionText raw =
  case T.stripSuffix ")" =<< T.stripPrefix "user error (" raw of
    Just inner -> inner
    Nothing -> raw

runSideEffect :: Model -> IO () -> Text -> IO GuiEvent
runSideEffect model action message =
  safeGuiTask $ do
    action
    pure model {notification = Just (Notification SuccessNotice message)}

runCopyRecentLog :: Model -> IO GuiEvent
runCopyRecentLog model =
  safeGuiTask $ do
    excerpt <- readRecentLogExcerpt 40
    copyTextToClipboard excerpt
    pure model {notification = Just (Notification SuccessNotice "Copied recent log lines.")}

readRecentLogExcerpt :: Int -> IO Text
readRecentLogExcerpt lineCount = do
  exists <- doesFileExist logPath
  if exists
    then do
      raw <- TIO.readFile logPath
      pure (T.unlines (takeLast lineCount (T.lines raw)))
    else pure "No GUI log file exists yet."

takeLast :: Int -> [a] -> [a]
takeLast count values =
  drop (max 0 (length values - count)) values

runCreateSupportBundle :: Model -> IO GuiEvent
runCreateSupportBundle model =
  safeGuiTask $ do
    bundlePath <- createSupportBundle model
    openExternalPath bundlePath
    pure model {notification = Just (Notification SuccessNotice ("Created support bundle: " <> T.pack bundlePath))}

createSupportBundle :: Model -> IO FilePath
createSupportBundle model = do
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
      bundlePath = "support_bundles" </> ("support-bundle-" <> stamp)
  createDirectoryIfMissing True bundlePath
  copyIfExists settingsPath (bundlePath </> "settings.json")
  copyIfExists dbPath (bundlePath </> "zeit-tool.db")
  copyIfExists logPath (bundlePath </> "app.log")
  TIO.writeFile (bundlePath </> "summary.txt") (supportBundleSummary now model)
  logInfo ("Created support bundle at " <> T.pack bundlePath)
  pure bundlePath

copyIfExists :: FilePath -> FilePath -> IO ()
copyIfExists source target = do
  exists <- doesFileExist source
  if exists
    then copyFile source target
    else pure ()

supportBundleSummary :: UTCTime -> Model -> Text
supportBundleSummary timestamp model =
  T.unlines
    [ "Zeit Tool Haskell support bundle"
    , "Created: " <> tshow timestamp
    , "View: " <> tshow (currentView model)
    , "Row density: " <> tshow (rowDensity model)
    , "UI theme: " <> tshow (uiTheme model)
    , "Browse section: " <> browseSectionId model
    , "Browse loaded: " <> tshow (length (browseArticles model))
    , "Library loaded: " <> tshow (length (libraryArticles model)) <> " of " <> tshow (libraryTotal model)
    , "LingQ loaded: " <> tshow (length (lingqArticles model))
    , "Failed fetches: " <> tshow (length (failedFetches model))
    , "Failed uploads: " <> tshow (length (failedUploads model))
    , "Queued jobs: " <> tshow (length (queuedJobs model))
    , "Queue paused: " <> tshow (jobQueuePaused model)
    , "Completed jobs: " <> tshow (length (completedJobs model))
    , "Known stems: " <> tshow (knownStemTotal model)
    ]

openExternalUrl :: Text -> IO ()
openExternalUrl url =
  case os of
    "mingw32" -> callProcess "cmd" ["/c", "start", "", T.unpack url]
    "darwin" -> callProcess "open" [T.unpack url]
    _ -> callProcess "xdg-open" [T.unpack url]

openExternalPath :: FilePath -> IO ()
openExternalPath path =
  case os of
    "mingw32" -> callProcess "explorer" [path]
    "darwin" -> callProcess "open" [path]
    _ -> callProcess "xdg-open" [path]

copyTextToClipboard :: Text -> IO ()
copyTextToClipboard =
  case os of
    "mingw32" -> pipeToProcess "powershell" ["-NoProfile", "-Command", "Set-Clipboard"]
    "darwin" -> pipeToProcess "pbcopy" []
    _ -> pipeToProcess "xclip" ["-selection", "clipboard"]

pipeToProcess :: FilePath -> [String] -> Text -> IO ()
pipeToProcess command args text =
  withCreateProcess (proc command args) {std_in = CreatePipe} $ \maybeInput _ _ processHandle -> do
    case maybeInput of
      Nothing -> pure ()
      Just input -> do
        TIO.hPutStr input text
        hClose input
    _ <- waitForProcess processHandle
    pure ()

articleCopyText :: Article -> Text
articleCopyText article =
  T.intercalate "\n\n" (filter (not . T.null) blocks)
  where
    blocks =
      [ articleTitle article
      , articleSubtitle article
      ]
        <> articleParagraphs article

guiBatchFetchSummary :: [BatchFetchResult] -> Text
guiBatchFetchSummary results =
  "Batch fetch: saved "
    <> tshow saved
    <> ", skipped "
    <> tshow skipped
    <> ", failed "
    <> tshow failed
    <> "."
  where
    saved = length [() | BatchSaved {} <- results]
    skipped = length [() | BatchSkipped {} <- results]
    failed = length [() | BatchFailed {} <- results]

batchFetchProgressDetail :: BatchFetchResult -> Text
batchFetchProgressDetail result =
  case result of
    BatchSaved url _ -> "Saved " <> url
    BatchSkipped url decision -> "Skipped " <> url <> " (" <> tshow decision <> ")"
    BatchFailed url err -> "Failed " <> url <> ": " <> err

guiBatchFetchFailures :: [BatchFetchResult] -> [(Text, Text)]
guiBatchFetchFailures results =
  [ (url, err)
  | BatchFailed url err <- results
  ]

guiBatchFetchLevel :: [BatchFetchResult] -> NotificationLevel
guiBatchFetchLevel results
  | any isFailed results = ErrorNotice
  | otherwise = SuccessNotice
  where
    isFailed BatchFailed {} = True
    isFailed _ = False

guiBatchUploadResultEvents :: Bool -> [(ArticleId, Text)] -> [BatchUploadResult] -> [Event]
guiBatchUploadResultEvents cancelled loadFailures results =
  [ Notify level
      (withCancelSuffix cancelled (guiBatchUploadSummary loadFailures results))
  ]
  where
    failed = length loadFailures + length [() | UploadFailed {} <- results]
    level
      | cancelled = InfoNotice
      | failed > 0 = ErrorNotice
      | otherwise = SuccessNotice

guiBatchUploadSummary :: [(ArticleId, Text)] -> [BatchUploadResult] -> Text
guiBatchUploadSummary loadFailures results =
  "Batch upload: uploaded "
    <> tshow uploaded
    <> ", failed "
    <> tshow failed
    <> "."
  where
    uploaded = length [() | UploadSucceeded {} <- results] + length [() | UploadSucceededUntracked {} <- results]
    failed = length loadFailures + length [() | UploadFailed {} <- results]

withCancelSuffix :: Bool -> Text -> Text
withCancelSuffix cancelled message
  | cancelled = message <> " Cancelled before processing the remaining items."
  | otherwise = message

guiBatchUploadFailures :: [BatchUploadResult] -> [(ArticleId, Text)]
guiBatchUploadFailures results =
  [ (ident, title <> ": " <> err)
  | UploadFailed (Just ident) title err <- results
  ]

batchUploadProgressDetail :: BatchUploadResult -> Text
batchUploadProgressDetail result =
  case result of
    UploadSucceeded _ title _ -> "Uploaded " <> title
    UploadSucceededUntracked title _ -> "Uploaded " <> title
    UploadFailed _ title err -> "Failed " <> title <> ": " <> err

makePorts :: IO (AppPorts IO)
makePorts = do
  pure
    AppPorts
      { zeitPort = guiZeitPort settingsPath
      , lingqPort = guiLingqPort settingsPath
      , audioPort = guiAudioPort
      , libraryPort = guiLibraryPort
      , settingsPort = jsonSettingsPort settingsPath
      }

guiZeitPort :: FilePath -> ZeitPort IO
guiZeitPort path =
  ZeitPort
    { fetchSections = pure allSections
    , fetchArticleList = \sectionIdent page -> do
        session <- loadZeitSession path
        either failWithShow pure =<< fetchArticleListZeit session sectionIdent page
    , fetchArticleContent = \url -> do
        session <- loadZeitSession path
        either failWithShow pure =<< fetchArticleContentZeit session url
    , loginToZeit = zeitStatusFromSettings path
    , loginToZeitWithCookie = \cookie -> do
        saveSettings path . (\settings -> settings {settingsZeitCookie = T.strip cookie}) =<< loadSettings path
        zeitStatusFromSettings path
    , loginToZeitWithBrowserSession = \cookie userAgent -> do
        saveSettings path
          . ( \settings ->
                settings
                  { settingsZeitCookie = T.strip cookie
                  , settingsZeitUserAgent = normalizeZeitUserAgent userAgent
                  }
            )
          =<< loadSettings path
        zeitStatusFromSettings path
    , logoutFromZeit =
        saveSettings path . (\settings -> settings {settingsZeitCookie = ""}) =<< loadSettings path
    }

guiLingqPort :: FilePath -> LingqPort IO
guiLingqPort path =
  LingqPort
    { loginToLingq = \username password ->
        if T.null (T.strip username) && T.null (T.strip password)
          then lingqStatusFromSettings path
          else do
            token <- either failWithShow pure =<< loginWithPasswordLingq username password
            saveLingqToken path token
            pure AuthStatus {authLoggedIn = True, authLabel = Just (nonEmptyOr "password login" username)}
    , loginToLingqWithApiKey = \apiKey -> do
        token <- either failWithShow pure =<< loginWithApiKeyLingq (T.strip apiKey)
        saveLingqToken path token
        pure AuthStatus {authLoggedIn = True, authLabel = Just "API key"}
    , logoutFromLingq =
        saveSettings path . (\settings -> settings {settingsLingqApiKey = ""}) =<< loadSettings path
    , uploadLessonToLingq = \languageCode collectionId article -> do
        token <- loadLingqToken path
        either failWithShow pure =<< uploadLessonLingq token languageCode collectionId article
    , updateLessonOnLingq = \languageCode existingLesson article -> do
        token <- loadLingqToken path
        either failWithShow pure =<< updateLessonLingq token languageCode existingLesson article
    , fetchLanguages = do
        token <- loadLingqToken path
        either failWithShow pure =<< fetchLanguagesLingq token
    , fetchCollections = \languageCode -> do
        token <- loadLingqToken path
        either failWithShow pure =<< fetchCollectionsLingq token languageCode
    , fetchCollectionLessons = \languageCode collectionId -> do
        token <- loadLingqToken path
        either failWithShow pure =<< fetchCollectionLessonsLingq token languageCode collectionId
    , fetchKnownWords = \languageCode -> do
        token <- loadLingqToken path
        either failWithShow pure =<< fetchKnownWordsLingq token languageCode
    }

loadZeitSession :: FilePath -> IO ZeitSession
loadZeitSession path =
  ZeitSession
    <$> loadConfiguredText path settingsZeitCookie "ZEIT_COOKIE"
    <*> loadConfiguredText path settingsZeitUserAgent "ZEIT_USER_AGENT"

zeitStatusFromSettings :: FilePath -> IO AuthStatus
zeitStatusFromSettings path = do
  cookie <- loadConfiguredText path settingsZeitCookie "ZEIT_COOKIE"
  pure
    AuthStatus
      { authLoggedIn = not (T.null cookie)
      , authLabel =
          if T.null cookie
            then Just "paste cookie"
            else Just "cookie session"
      }

loadLingqToken :: FilePath -> IO LingqToken
loadLingqToken path = do
  apiKey <- loadConfiguredText path settingsLingqApiKey "LINGQ_API_KEY"
  if T.null apiKey
    then fail "Connect a LingQ API key before using LingQ actions."
    else pure (LingqToken apiKey)

lingqStatusFromSettings :: FilePath -> IO AuthStatus
lingqStatusFromSettings path = do
  apiKey <- loadConfiguredText path settingsLingqApiKey "LINGQ_API_KEY"
  pure
    AuthStatus
      { authLoggedIn = not (T.null apiKey)
      , authLabel =
          if T.null apiKey
            then Just "connect API key"
            else Just "API key"
      }

saveLingqToken :: FilePath -> LingqToken -> IO ()
saveLingqToken path (LingqToken token) =
  saveSettings path . (\settings -> settings {settingsLingqApiKey = token}) =<< loadSettings path

loadConfiguredText :: FilePath -> (Settings -> Text) -> String -> IO Text
loadConfiguredText path select envName = do
  settings <- loadSettings path
  envValue <- fmap T.pack <$> lookupEnv envName
  pure (maybe "" id (nonEmptyMaybe (select settings) <|> envValueMaybe envValue))
  where
    envValueMaybe maybeValue =
      case T.strip <$> maybeValue of
        Just value | not (T.null value) -> Just value
        _ -> Nothing

nonEmptyMaybe :: Text -> Maybe Text
nonEmptyMaybe value
  | T.null stripped = Nothing
  | otherwise = Just stripped
  where
    stripped = T.strip value

nonEmptyOr :: Text -> Text -> Text
nonEmptyOr fallback value
  | T.null stripped = fallback
  | otherwise = stripped
  where
    stripped = T.strip value

guiAudioPort :: AudioPort IO
guiAudioPort =
  AudioPort
    { downloadArticleAudioFile = \audioDir article ->
        either failWithShow pure =<< downloadArticleAudio audioDir article
    , openAudioFile = openAudioPath
    }

guiLibraryPort :: LibraryPort IO
guiLibraryPort =
  LibraryPort
    { loadLibrary = \filters ->
        withLibrary dbPath $ \db -> getArticlesSqlite db filters
    , loadLibraryPage = \query ->
        withLibrary dbPath $ \db -> getArticlesByQuerySqlite db query
    , loadArticle = \ident ->
        withLibrary dbPath $ \db -> getArticleSqlite db ident
    , saveArticle = \article ->
        withLibrary dbPath $ \db -> saveArticleSqlite db article
    , deleteArticle = \ident ->
        withLibrary dbPath $ \db -> deleteArticleSqlite db ident
    , setArticleIgnored = \ident ignored ->
        withLibrary dbPath $ \db -> setIgnoredSqlite db ident ignored
    , markArticleUploaded = \ident lesson ->
        withLibrary dbPath $ \db -> markUploadedSqlite db ident lesson
    , setArticleAudioPath = \ident path ->
        withLibrary dbPath $ \db -> setAudioPathSqlite db ident path
    , loadIgnoredUrls =
        withLibrary dbPath getIgnoredUrlsSqlite
    , ignoreArticleUrl = \url ->
        withLibrary dbPath $ \db -> ignoreUrlSqlite db url
    , unignoreArticleUrl = \url ->
        withLibrary dbPath $ \db -> unignoreUrlSqlite db url
    , deleteIgnoredArticles =
        withLibrary dbPath deleteIgnoredSqlite
    , deleteOlderArticles = \cutoff onlyUploaded onlyUnuploaded ->
        withLibrary dbPath $ \db -> deleteOlderThanSqlite db cutoff onlyUploaded onlyUnuploaded
    , replaceKnownWords = \languageCode stems ->
        withLibrary dbPath $ \db -> saveKnownWordsSqlite db languageCode stems
    , addKnownWords = \languageCode stems ->
        withLibrary dbPath $ \db -> addKnownWordsSqlite db languageCode stems
    , clearKnownWords = \languageCode ->
        withLibrary dbPath $ \db -> clearKnownWordsSqlite db languageCode
    , clearKnownPercentages =
        withLibrary dbPath clearAllKnownPctSqlite
    , computeKnownPercentages = \languageCode ->
        withLibrary dbPath $ \db -> computeKnownPctSqlite db languageCode
    , knownStemCount = \languageCode ->
        withLibrary dbPath $ \db -> getKnownStemCountSqlite db languageCode
    , loadStats =
        withLibrary dbPath getStatsSqlite
    }

failWithShow :: Show err => err -> IO a
failWithShow = fail . show

parseOptionalInt :: Text -> Maybe Int
parseOptionalInt raw =
  case T.unpack (T.strip raw) of
    "" -> Nothing
    value ->
      case reads value of
        [(parsed, "")] | parsed >= (0 :: Int) -> Just parsed
        _ -> Nothing

parsePositiveInt :: Text -> Maybe Int
parsePositiveInt raw =
  case reads (T.unpack (T.strip raw)) of
    [(parsed, "")] | parsed > (0 :: Int) -> Just parsed
    _ -> Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show

updateSectionCollection :: Text -> Text -> Map.Map Text Text -> Map.Map Text Text
updateSectionCollection sectionName raw mappings
  | T.null collectionId = Map.delete sectionName mappings
  | otherwise = Map.insert sectionName collectionId mappings
  where
    collectionId = T.strip raw

selectedBrowseArticles :: Model -> [ArticleSummary] -> [ArticleSummary]
selectedBrowseArticles model =
  filter (\article -> Set.member (summaryUrl article) (browseSelectedUrls model))

selectedLingqArticles :: Model -> [ArticleSummary] -> [ArticleSummary]
selectedLingqArticles model =
  filter isSelected
  where
    isSelected article =
      maybe False (\ident -> Set.member ident (lingqSelectedIds model)) (summaryId article)

uploadableSummary :: ArticleSummary -> Bool
uploadableSummary article =
  not (summaryUploaded article) && not (summaryIgnored article)

mapMaybeSummaryId :: [ArticleSummary] -> [ArticleId]
mapMaybeSummaryId articles =
  [ ident
  | article <- articles
  , Just ident <- [summaryId article]
  ]

failedFetchSummary :: (Text, Text) -> ArticleSummary
failedFetchSummary (url, _) =
  ArticleSummary
    { summaryId = Nothing
    , summaryUrl = url
    , summaryTitle = url
    , summarySection = ""
    , summaryWordCount = 0
    , summaryIgnored = False
    , summaryUploaded = False
    , summaryKnownPct = Nothing
    }

failedUploadSummary :: (ArticleId, Text) -> ArticleSummary
failedUploadSummary (ident, labelText) =
  ArticleSummary
    { summaryId = Just ident
    , summaryUrl = ""
    , summaryTitle = labelText
    , summarySection = ""
    , summaryWordCount = 0
    , summaryIgnored = False
    , summaryUploaded = False
    , summaryKnownPct = Nothing
    }

main :: IO ()
main = do
  ports <- makePorts
  cancelFlag <- newIORef False
  initial <- loadInitialModel (settingsPort ports)
  startApp initial (handleEvent (GuiRuntime ports cancelFlag)) buildUI (config initial)
  where
    config initial =
      [ appWindowTitle "Zeit Tool Haskell"
      , appWindowState MainWindowMaximized
      , appWindowResizable True
      , appTheme (monomerThemeFor (uiTheme initial))
      , appFontDef "Regular" "C:\\Windows\\Fonts\\segoeui.ttf"
      , appInitEvent GuiInit
      ]
