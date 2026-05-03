{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime, utctDay)
import Monomer hiding (Model)
import Monomer qualified as M
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Info (os)
import System.IO (hClose)
import System.Process (CreateProcess(std_in), StdStream(CreatePipe), callProcess, proc, waitForProcess, withCreateProcess)
import Data.Text.IO qualified as TIO
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.Core.Batch (BatchFetchResult(..))
import ZeitLingq.Core.Upload (BatchUploadConfig(..), BatchUploadResult(..), targetCollectionFor)
import ZeitLingq.Domain.Article (BatchDecision(..), applyWordFilter, lessonTitle)
import ZeitLingq.App.Driver (dispatchEvent, dispatchEvents)
import ZeitLingq.App.Model (Model(..), initialModel)
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

data GuiEvent
  = GuiInit
  | GuiModelLoaded Model
  | GuiFailed Text
  | GuiNavigate View
  | GuiRefresh
  | GuiZeitCookieChanged Text
  | GuiOpenZeitLoginPage
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
  | GuiProgress (Maybe ProgressStatus)
  | GuiRowDensityChanged RowDensity
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

appBgColor :: Color
appBgColor = rgbHex "#0f151b"

panelBgColor :: Color
panelBgColor = rgbHex "#17212b"

panelAltColor :: Color
panelAltColor = rgbHex "#101923"

borderColor :: Color
borderColor = rgbHex "#263746"

primaryColor :: Color
primaryColor = rgbHex "#16d9c5"

primaryTextColor :: Color
primaryTextColor = rgbHex "#071113"

mainTextColor :: Color
mainTextColor = rgbHex "#f3f7f8"

mutedTextColor :: Color
mutedTextColor = rgbHex "#9eadba"

warningColor :: Color
warningColor = rgbHex "#f5a524"

dangerColor :: Color
dangerColor = rgbHex "#ff6b6b"

buildUI :: WidgetEnv Model GuiEvent -> Model -> WidgetNode Model GuiEvent
buildUI _ model =
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
    `styleBasic` [bgColor appBgColor]
  where
    vm = appViewModel model

handleEvent
  :: AppPorts IO
  -> WidgetEnv Model GuiEvent
  -> WidgetNode Model GuiEvent
  -> Model
  -> GuiEvent
  -> [AppEventResponse Model GuiEvent]
handleEvent ports _ _ model event =
  case event of
    GuiInit ->
      [Task (loadGuiInitialModel ports)]
    GuiModelLoaded nextModel ->
      [M.Model nextModel]
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
        selected ->
          withProgressProducer
            model
            "Fetching selected articles"
            (length selected)
            (runFetchBatchProducer ports model selected)
    GuiFetchVisible articles ->
      withProgressProducer
        model
        "Fetching visible articles"
        (length articles)
        (runFetchBatchProducer ports model articles)
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
        selected ->
          withProgressProducer
            model
            "Uploading selected articles"
            (length selected)
            (runUploadBatchProducer ports model selected)
    GuiUploadVisible articles ->
      withProgressProducer
        model
        "Uploading visible articles"
        (length articles)
        (runUploadBatchProducer ports model articles)
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
          withProgressProducer
            model
            "Retrying failed fetches"
            (length failures)
            (runFetchBatchProducer ports model (map failedFetchSummary failures))
    GuiRetryFailedUploads ->
      case failedUploads model of
        [] -> [Task (runAppEvent ports model (Notify InfoNotice "No failed uploads to retry."))]
        failures ->
          withProgressProducer
            model
            "Retrying failed uploads"
            (length failures)
            (runUploadBatchProducer ports model (map failedUploadSummary failures))
    GuiClearFailures ->
      [Task (runAppEvent ports model FailureListsCleared)]
    GuiProgress progress ->
      let (nextModel, _) = update (ProgressChanged progress) model
       in [M.Model nextModel]
    GuiRowDensityChanged density ->
      [Task (runAppEvent ports model (RowDensityChanged density))]
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
    GuiCloseArticle ->
      [Task (runAppEvent ports model ArticleClosed)]
    GuiClearNotice ->
      [Task (runAppEvent ports model NotificationCleared)]

withPendingNotice :: Model -> Text -> IO GuiEvent -> [AppEventResponse Model GuiEvent]
withPendingNotice model message task =
  [ M.Model model {notification = Just (Notification InfoNotice message)}
  , Task task
  ]

withProgressProducer :: Model -> Text -> Int -> ((GuiEvent -> IO ()) -> IO ()) -> [AppEventResponse Model GuiEvent]
withProgressProducer model labelText total producer =
  [ M.Model
      model
        { notification = Just (Notification InfoNotice labelText)
        , activeProgress = Just (ProgressStatus labelText 0 total "")
        }
  , Producer producer
  ]

titleBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
titleBlock model vm =
  hstack
    [ label (vmTitle vm)
        `styleBasic` [textSize 20, textColor mainTextColor, paddingR 14]
    , filler
    , statusBlock vm
    , label "Density"
        `styleBasic` [paddingL 12, paddingR 6, textColor mutedTextColor]
    , textDropdownV_
        (rowDensity model)
        GuiRowDensityChanged
        allRowDensities
        rowDensityLabel
        []
        `styleBasic` (inputStyle <> [width 126])
    , secondaryButton "Refresh" GuiRefresh
    ]
    `styleBasic` [paddingB 6]

sidebarBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
sidebarBlock model vm =
  vstack
    [ label "Zeit Reader"
        `styleBasic` [textSize 20, textColor mainTextColor, paddingB 2]
    , mutedLabel "Haskell workflow"
        `styleBasic` [paddingB 14]
    , vstack (map sideNavButton (vmNavItems vm))
    , sidebarStatusBlock vm
        `styleBasic` [paddingT 12]
    , sidebarLibraryStats model
        `styleBasic` [paddingT 12]
    , sidebarProgressBlock model
        `styleBasic` [paddingT 12]
    , sidebarFailureBlock model
        `styleBasic` [paddingT 12]
    , filler
    , secondaryButton "Open data folder" GuiOpenDataFolder
    , secondaryButton "Open logs" GuiOpenLogs
        `styleBasic` [paddingT 6]
    , secondaryButton "Refresh" GuiRefresh
        `styleBasic` [paddingT 6]
    ]
    `styleBasic` [width 202, padding 12, bgColor panelAltColor, border 1 borderColor]

sideNavButton :: NavItem -> WidgetNode Model GuiEvent
sideNavButton item =
  button (navLabel item) (GuiNavigate (navView item))
    `styleBasic`
      [ paddingH 10
      , paddingV 3
      , height 30
      , width 176
      , radius 8
      , textSize 13
      , textColor (if navActive item then primaryTextColor else mainTextColor)
      , bgColor (if navActive item then primaryColor else panelBgColor)
      , border 1 (if navActive item then primaryColor else borderColor)
      , paddingB 6
      ]

sidebarStatusBlock :: AppViewModel -> WidgetNode Model GuiEvent
sidebarStatusBlock vm =
  vstack
    ( mutedLabel "Connections"
        : map statusLabel (vmStatusBadges vm)
    )
    `styleBasic` [padding 10, radius 12, bgColor panelBgColor, border 1 borderColor]

sidebarLibraryStats :: Model -> WidgetNode Model GuiEvent
sidebarLibraryStats model =
  case libraryStats model of
    Nothing -> emptyBlock
    Just stats ->
      vstack
        [ mutedLabel "Library"
        , sidebarStat "Articles" (totalArticles stats)
        , sidebarStat "Uploaded" (uploadedArticles stats)
        , sidebarStat "Avg words" (averageWordCount stats)
        ]
        `styleBasic` [padding 10, radius 12, bgColor panelBgColor, border 1 borderColor]

sidebarStat :: Text -> Int -> WidgetNode Model GuiEvent
sidebarStat name value =
  hstack
    [ mutedLabel name
    , filler
    , label (tshow value)
        `styleBasic` [textSize 13, textColor mainTextColor]
    ]
    `styleBasic` [paddingT 4]

sidebarProgressBlock :: Model -> WidgetNode Model GuiEvent
sidebarProgressBlock model =
  case activeProgress model of
    Nothing -> emptyBlock
    Just progress ->
      vstack
        [ mutedLabel "Current job"
        , label_ (progressLabel progress) [ellipsis]
            `styleBasic` [textSize 12, textColor mainTextColor, paddingT 4]
        , progressMeter 158 progress
            `styleBasic` [paddingT 7]
        , label (progressCountText progress)
            `styleBasic` [textSize 11, textColor mutedTextColor, paddingT 5]
        , label_ (progressDetail progress) [ellipsis]
            `styleBasic` [textSize 10, textColor mutedTextColor, paddingT 3]
        ]
        `styleBasic` [padding 10, radius 12, bgColor panelBgColor, border 1 borderColor]

sidebarFailureBlock :: Model -> WidgetNode Model GuiEvent
sidebarFailureBlock model
  | null (failedFetches model) && null (failedUploads model) = emptyBlock
  | otherwise =
      vstack
        [ mutedLabel "Retry list"
        , failureCountLine "Fetch" (length (failedFetches model))
        , failureCountLine "Upload" (length (failedUploads model))
        , vstack (map failureLine (take 3 (map fst (failedFetches model) <> map (tshow . fst) (failedUploads model))))
        , hstack
            [ rowSecondaryButton "Retry fetch" GuiRetryFailedFetches
            , rowSecondaryButton "Retry upload" GuiRetryFailedUploads
            ]
            `styleBasic` [paddingT 6]
        , rowDangerButton "Clear" GuiClearFailures
            `styleBasic` [paddingT 4]
        ]
        `styleBasic` [padding 10, radius 12, bgColor panelBgColor, border 1 borderColor]

failureCountLine :: Text -> Int -> WidgetNode Model GuiEvent
failureCountLine name value =
  hstack
    [ mutedLabel name
    , filler
    , label (tshow value)
        `styleBasic` [textSize 12, textColor (if value > 0 then dangerColor else mutedTextColor)]
    ]
    `styleBasic` [paddingT 4]

failureLine :: Text -> WidgetNode Model GuiEvent
failureLine text =
  label_ text [ellipsis]
    `styleBasic` [textSize 10, textColor mutedTextColor, paddingT 3]

statusBlock :: AppViewModel -> WidgetNode Model GuiEvent
statusBlock vm =
  hstack (map statusLabel (vmStatusBadges vm))

statusLabel :: StatusBadge -> WidgetNode Model GuiEvent
statusLabel badge =
  label (statusName badge <> ": " <> statusText badge)
    `styleBasic`
      [ paddingH 10
      , paddingV 5
      , paddingR 12
      , radius 12
      , textSize 13
      , textColor (if statusConnected badge then primaryColor else warningColor)
      , bgColor panelAltColor
      ]

notificationBlock :: Model -> WidgetNode Model GuiEvent
notificationBlock model =
  case notification model of
    Nothing -> emptyBlock
    Just notice ->
      hstack
        [ label (noticeText notice)
            `styleBasic` [textColor (noticeColor notice)]
        , filler
        , secondaryButton "Clear" GuiClearNotice
        ]
        `styleBasic` [padding 10, bgColor panelBgColor, radius 10, border 1 borderColor, paddingB 8]

progressNoticeBlock :: Model -> WidgetNode Model GuiEvent
progressNoticeBlock model =
  case activeProgress model of
    Nothing -> emptyBlock
    Just progress ->
      vstack
        [ hstack
            [ label (progressLabel progress)
                `styleBasic` [textColor mainTextColor]
            , filler
            , label (progressCountText progress)
                `styleBasic` [textColor mutedTextColor, textSize 12]
            ]
        , progressMeter 360 progress
            `styleBasic` [paddingT 7]
        , label_ (progressDetail progress) [ellipsis]
            `styleBasic` [textColor mutedTextColor, textSize 11, paddingT 5]
        ]
        `styleBasic` [padding 10, bgColor panelBgColor, radius 10, border 1 borderColor, paddingB 8]

progressMeter :: Double -> ProgressStatus -> WidgetNode Model GuiEvent
progressMeter meterWidth progress =
  zstack
    [ spacer
        `styleBasic` [height 7, width meterWidth, radius 4, bgColor borderColor]
    , spacer
        `styleBasic` [height 7, width fillWidth, radius 4, bgColor primaryColor]
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

primaryButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
primaryButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 8
      , paddingV 2
      , height 28
      , radius 7
      , textSize 12
      , textColor primaryTextColor
      , bgColor primaryColor
      , border 1 primaryColor
      ]

secondaryButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
secondaryButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 8
      , paddingV 2
      , height 28
      , radius 7
      , textSize 12
      , textColor mainTextColor
      , bgColor panelAltColor
      , border 1 borderColor
      ]

dangerButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
dangerButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 8
      , paddingV 2
      , height 28
      , radius 7
      , textSize 12
      , textColor mainTextColor
      , bgColor (rgbHex "#3a1f24")
      , border 1 dangerColor
      ]

rowPrimaryButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
rowPrimaryButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor primaryTextColor
      , bgColor primaryColor
      , border 1 primaryColor
      ]

rowSecondaryButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
rowSecondaryButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor mainTextColor
      , bgColor panelAltColor
      , border 1 borderColor
      ]

rowDangerButton :: Text -> GuiEvent -> WidgetNode Model GuiEvent
rowDangerButton caption event =
  button caption event
    `styleBasic`
      [ paddingH 5
      , paddingV 1
      , height 20
      , radius 6
      , textSize 10
      , textColor mainTextColor
      , bgColor (rgbHex "#3a1f24")
      , border 1 dangerColor
      ]

inputStyle :: [StyleState]
inputStyle =
  [ paddingH 8
  , paddingV 2
  , height 28
  , radius 7
  , textSize 12
  , textColor mainTextColor
  , bgColor panelAltColor
  , border 1 borderColor
  ]

panelStyle :: [StyleState]
panelStyle =
  [ padding 8
  , radius 12
  , bgColor panelBgColor
  , border 1 borderColor
  ]

mutedLabel :: Text -> WidgetNode Model GuiEvent
mutedLabel caption =
  label caption
    `styleBasic` [textSize 13, textColor mutedTextColor]

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
    [ screenSummary vm
    , zeitControls model
    , browseControls model
    , libraryControls model
    , lingqControls model
    , selectedArticleBlock model (vmSelectedArticle vm)
    , articleParagraphsBlock (vmSelectedArticleParagraphs vm)
    , articleRowsBlock model (rowsForCurrentView model)
    ]
    `styleBasic` panelStyle

screenSummary :: AppViewModel -> WidgetNode Model GuiEvent
screenSummary vm =
  hstack
    [ mutedLabel ("Section: " <> vmBrowseSection vm)
    , mutedLabel ("Filter: " <> vmActiveFilter vm)
        `styleBasic` [paddingL 16]
    , mutedLabel (vmDatePrefix vm)
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
        , label "Paste your zeit.de Cookie header here. This keeps credentials out of the app while still making paid article fetches GUI-configurable."
            `styleBasic` [textSize 12, paddingB 6]
        , textAreaV_
            (zeitCookieText model)
            GuiZeitCookieChanged
            [maxLines 5]
            `styleBasic` (inputStyle <> [height 110])
        , hstack
            [ secondaryButton "Open Zeit login page" GuiOpenZeitLoginPage
            , primaryButton "Save cookie session" GuiZeitCookieLogin
            , secondaryButton "Disconnect Zeit" GuiZeitLogout
            ]
            `styleBasic` [paddingT 6]
        ]
    _ -> emptyBlock

browseControls :: Model -> WidgetNode Model GuiEvent
browseControls model =
  case currentView model of
    BrowseView ->
      vstack
        [ hstack
            [ label "Topic"
                `styleBasic` [paddingR 8, textColor mutedTextColor]
            , textDropdownV_
                (browseSectionId model)
                GuiSectionSelected
                (map sectionId allSections)
                sectionLabelForId
                [maxHeight 360]
                `styleBasic` (inputStyle <> [width 240])
            , label "Search"
                `styleBasic` [paddingL 12, paddingR 6, textColor mutedTextColor]
            , textFieldV_ (browseSearch model) GuiBrowseSearchChanged [placeholder "title or topic"]
                `styleBasic` (inputStyle <> [width 190])
            , secondaryButton "Previous" GuiBrowsePreviousPage
            , label ("Page " <> tshow (browsePage model))
                `styleBasic` [paddingH 10, textColor mainTextColor]
            , secondaryButton "Next" GuiBrowseNextPage
            , label "Fetch min"
                `styleBasic` [paddingL 14, paddingR 6, textColor mutedTextColor]
            , textFieldV (maybe "" tshow (minWords (browseFilter model))) GuiBrowseMinWordsChanged
                `styleBasic` (inputStyle <> [width 72])
            , label "max"
                `styleBasic` [paddingL 10, paddingR 6, textColor mutedTextColor]
            , textFieldV (maybe "" tshow (maxWords (browseFilter model))) GuiBrowseMaxWordsChanged
                `styleBasic` (inputStyle <> [width 72])
            ]
            `styleBasic` [paddingB 8]
        , hstack
            [ toggle "Only new" (browseOnlyNew model) GuiBrowseOnlyNewChanged
            , toggle "Show hidden" (browseShowHidden model) GuiBrowseShowHiddenChanged
            , secondaryButton "Select shown" (GuiBrowseSelectVisible visible)
            , secondaryButton "Deselect" GuiBrowseClearSelection
            , label (tshow (Set.size (browseSelectedUrls model)) <> " selected")
                `styleBasic` [paddingL 8, paddingR 12, textColor mutedTextColor]
            , primaryButton ("Fetch selected (" <> tshow (Set.size (browseSelectedUrls model)) <> ")") (GuiFetchSelected visible)
            , secondaryButton ("Fetch shown (" <> tshow (length visible) <> ")") (GuiFetchVisible visible)
            , mutedLabel (tshow (length visible) <> " shown / " <> tshow (length (browseArticles model)) <> " loaded")
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
                `styleBasic` [paddingR 8, textColor mutedTextColor]
            , textDropdownV_
                (libraryPreset model)
                GuiLibraryPresetChanged
                allLibraryPresets
                libraryPresetLabel
                []
                `styleBasic` (inputStyle <> [width 160])
            , label "Search"
                `styleBasic` [paddingL 12, paddingR 8, textColor mutedTextColor]
            , textFieldV_ (maybe "" id (librarySearch query)) GuiLibrarySearchChanged [placeholder "Title or article text"]
                `styleBasic` (inputStyle <> [width 260])
            , label "Min"
                `styleBasic` [paddingL 12, paddingR 6, textColor mutedTextColor]
            , textFieldV (maybe "" tshow (minWords (libraryWordFilter query))) GuiLibraryMinWordsChanged
                `styleBasic` (inputStyle <> [width 70])
            , label "Max"
                `styleBasic` [paddingL 12, paddingR 6, textColor mutedTextColor]
            , textFieldV (maybe "" tshow (maxWords (libraryWordFilter query))) GuiLibraryMaxWordsChanged
                `styleBasic` (inputStyle <> [width 70])
            , label "Sort"
                `styleBasic` [paddingL 12, paddingR 6, textColor mutedTextColor]
            , textDropdownV_
                (librarySort query)
                GuiLibrarySortChanged
                allLibrarySorts
                librarySortLabel
                []
                `styleBasic` (inputStyle <> [width 130])
            ]
        , librarySectionControls model
        , hstack
            [ toggle "Show ignored" (libraryIncludeIgnored query) GuiLibraryIncludeIgnoredChanged
            , toggle "Only ignored" (libraryOnlyIgnored query) GuiLibraryOnlyIgnoredChanged
            , toggle "Only not uploaded" (libraryOnlyNotUploaded query) GuiLibraryOnlyNotUploadedChanged
            , toggle "Group by section" (libraryGroupBySection model) GuiLibraryGroupBySectionChanged
            ]
            `styleBasic` [paddingV 8]
        , hstack
            [ secondaryButton "Previous" GuiLibraryPreviousPage
            , label (libraryPageLabel model)
                `styleBasic` [paddingH 12, textColor mainTextColor]
            , secondaryButton "Next" GuiLibraryNextPage
            ]
        , hstack
            [ dangerButton "Delete ignored" GuiDeleteIgnoredArticles
            , label "Older than"
                `styleBasic` [paddingL 10, paddingR 6, textColor mutedTextColor]
            , textFieldV (libraryDeleteDaysText model) GuiLibraryDeleteDaysChanged
                `styleBasic` (inputStyle <> [width 56])
            , mutedLabel "days"
            , dangerButton "Delete old" (GuiDeleteOldArticles False False)
            , dangerButton "Delete uploaded" (GuiDeleteOldArticles True False)
            , dangerButton "Delete unuploaded" (GuiDeleteOldArticles False True)
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
        [ statLabel "Articles" (totalArticles stats)
        , statLabel "Uploaded" (uploadedArticles stats)
        , statLabel "Avg words" (averageWordCount stats)
        ]
        `styleBasic` [paddingB 8]

statLabel :: Text -> Int -> WidgetNode Model GuiEvent
statLabel name value =
  vstack
    [ label (tshow value)
        `styleBasic` [textSize 18, textColor primaryColor]
    , label name
        `styleBasic` [textSize 11, textColor mutedTextColor]
    ]
    `styleBasic` [paddingR 24]

librarySectionControls :: Model -> WidgetNode Model GuiEvent
librarySectionControls model =
  case libraryStats model of
    Nothing -> emptyBlock
    Just _stats ->
      hstack
        [ label "Section"
            `styleBasic` [paddingR 8, textColor mutedTextColor]
        , textDropdownV_
            activeSectionValue
            GuiLibrarySectionChanged
            ("" : map fst sections)
            sectionText
            [maxHeight 320]
            `styleBasic` (inputStyle <> [width 300])
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
    LibraryPresetCustom -> "Custom"

allRowDensities :: [RowDensity]
allRowDensities = [CompactRows, ComfortableRows]

rowDensityLabel :: RowDensity -> Text
rowDensityLabel CompactRows = "Compact"
rowDensityLabel ComfortableRows = "Comfort"

toggle :: Text -> Bool -> (Bool -> GuiEvent) -> WidgetNode Model GuiEvent
toggle text value handler =
  hstack
    [ checkboxV value handler
    , label text
        `styleBasic` [paddingL 5, paddingR 14, textColor mainTextColor]
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
            [ secondaryButton ("Select not uploaded (" <> tshow (length uploadable) <> ")") (GuiLingqSelectNotUploaded (lingqArticles model))
            , secondaryButton "Deselect" GuiLingqClearSelection
            , label (tshow (Set.size (lingqSelectedIds model)) <> " selected")
                `styleBasic` [paddingH 8, textColor mutedTextColor]
            , primaryButton ("Upload selected (" <> tshow (Set.size (lingqSelectedIds model)) <> ")") (GuiUploadSelected (lingqArticles model))
            , secondaryButton ("Upload visible (" <> T.pack (show (length uploadable)) <> ")") (GuiUploadVisible (lingqArticles model))
            , secondaryButton "Sync status" GuiSyncLingqStatus
            ]
            `styleBasic` [paddingB 6]
        , hstack
            [ secondaryButton "Sync known words" GuiSyncKnownWords
            , secondaryButton "Refresh %" GuiComputeKnownWords
            , secondaryButton "Refresh collections" GuiRefreshCollections
            , toggle "Auto-date lesson titles" (datePrefixEnabled model) GuiDatePrefixChanged
            ]
        , label ("Known stems (" <> lingqLanguage model <> "): " <> tshow (knownStemTotal model))
            `styleBasic` [paddingT 8, textColor mutedTextColor]
        , hstack
            [ secondaryButton
                (if lingqShowKnownImport model then "Hide known-word import" else "Import known words")
                (GuiLingqKnownImportVisible (not (lingqShowKnownImport model)))
            , dangerButton "Clear known words" GuiClearKnownWords
            , secondaryButton
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
    [ toggle "Only not uploaded" (lingqOnlyNotUploaded model) GuiLingqOnlyNotUploadedChanged
    , label "Min"
        `styleBasic` [paddingL 10, paddingR 6, textColor mutedTextColor]
    , textFieldV (maybe "" tshow (minWords (lingqFilter model))) GuiLingqMinWordsChanged
        `styleBasic` (inputStyle <> [width 70])
    , label "Max"
        `styleBasic` [paddingL 10, paddingR 6, textColor mutedTextColor]
    , textFieldV (maybe "" tshow (maxWords (lingqFilter model))) GuiLingqMaxWordsChanged
        `styleBasic` (inputStyle <> [width 70])
    , mutedLabel (tshow (length (lingqArticles model)) <> " upload candidates")
    ]
    `styleBasic` [paddingB 8]

lingqTargetControls :: Model -> WidgetNode Model GuiEvent
lingqTargetControls model =
  hstack
    [ label "Language"
        `styleBasic` [paddingR 8, textColor mutedTextColor]
    , textDropdownV_
        (lingqLanguage model)
        GuiLingqLanguageChanged
        (languageOptions model)
        (languageLabelFor model)
        [maxHeight 320]
        `styleBasic` (inputStyle <> [width 210])
    , secondaryButton "Refresh languages" GuiRefreshLanguages
    , label "Course"
        `styleBasic` [paddingL 16, paddingR 8, textColor mutedTextColor]
    , collectionPicker model
    , mutedLabel (if null (lingqCollections model) then "Refresh collections to use names." else "Blank keeps lessons standalone.")
    ]
    `styleBasic` [paddingB 8]

collectionPicker :: Model -> WidgetNode Model GuiEvent
collectionPicker model =
  if null (lingqCollections model)
    then textFieldV_
          (maybe "" id (lingqFallbackCollection model))
          GuiFallbackCollectionChanged
          [placeholder "collection id or blank"]
          `styleBasic` (inputStyle <> [width 230])
    else textDropdownV_
          (maybe "" id (lingqFallbackCollection model))
          GuiFallbackCollectionChanged
          ("" : map collectionId (lingqCollections model))
          (collectionLabelFor model)
          [maxHeight 320]
          `styleBasic` (inputStyle <> [width 320])

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
            `styleBasic` [paddingT 8, textColor mainTextColor]
        , textAreaV_
            (knownImportText model)
            GuiKnownImportTextChanged
            [maxLines 6]
            `styleBasic` (inputStyle <> [height 110])
        , primaryButton "Import pasted words" GuiImportKnownWords
            `styleBasic` [paddingT 6]
        ]

sectionMappingsPanel :: Model -> WidgetNode Model GuiEvent
sectionMappingsPanel model
  | not (lingqShowSectionMappings model) = emptyBlock
  | otherwise =
      vscroll (vstack ((label "Per-section LingQ collections" `styleBasic` [textColor mainTextColor, paddingB 6]) : map collectionRow allSections))
        `styleBasic` [height 260, paddingT 8]
  where
    collectionRow section =
      hstack
        [ label (sectionLabel section)
            `styleBasic` [width 150, textColor mutedTextColor]
        , textDropdownV_
            (Map.findWithDefault "" (sectionLabel section) (sectionCollections model))
            (GuiSectionCollectionChanged (sectionLabel section))
            ("" : map collectionId (lingqCollections model))
            (collectionLabelFor model)
            [maxHeight 260]
            `styleBasic` (inputStyle <> [width 320])
        ]
        `styleBasic` [paddingB 4]

lingqLoginControls :: Model -> WidgetNode Model GuiEvent
lingqLoginControls model =
  vstack
    [ label "LingQ connection"
        `styleBasic` [textSize 16, paddingT 8, textColor mainTextColor]
    , hstack
        [ label "API key"
            `styleBasic` [paddingR 8, textColor mutedTextColor]
        , textFieldV_
            (lingqApiKeyText model)
            GuiLingqApiKeyChanged
            [placeholder "LingQ API key"]
            `styleBasic` (inputStyle <> [width 360])
        , primaryButton "Connect API key" GuiLingqApiKeyLogin
        , secondaryButton "Disconnect" GuiLingqLogout
        ]
    , hstack
        [ label "Username"
            `styleBasic` [paddingR 8, textColor mutedTextColor]
        , textFieldV_
            (lingqUsernameText model)
            GuiLingqUsernameChanged
            [placeholder "LingQ username or email"]
            `styleBasic` (inputStyle <> [width 230])
        , label "Password"
            `styleBasic` [paddingL 12, paddingR 8, textColor mutedTextColor]
        , textFieldV_
            (lingqPasswordText model)
            GuiLingqPasswordChanged
            [placeholder "Password", textFieldDisplayChar '*']
            `styleBasic` (inputStyle <> [width 220])
        , primaryButton "Login with password" GuiLingqPasswordLogin
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
        `styleBasic` [textSize 18, paddingT 8, textColor mainTextColor]
    , label (rowMeta row)
        `styleBasic` [textColor mutedTextColor]
    , label (rowKnownPct row <> " | " <> rowUploadStatus row)
        `styleBasic` [textColor mutedTextColor]
    , hstack (articleButtons (selectedArticle model) (selectedArticleContent model))
        `styleBasic` [paddingT 6]
    ]

articleButtons :: Maybe ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
articleButtons Nothing _ = [secondaryButton "Back to library" GuiCloseArticle]
articleButtons (Just article) maybeContent =
  [ secondaryButton "Back to library" GuiCloseArticle
  , secondaryButton "Original" (GuiOpenExternal (summaryUrl article))
  ]
    <> maybe [] (\content -> [secondaryButton "Copy text" (GuiCopyText (articleCopyText content))]) maybeContent
    <> maybe [] (\ident -> uploadAction ident article) (summaryId article)
    <> audioButtons article maybeContent
    <> maybe [] (\ident -> [dangerButton "Delete" (GuiDeleteArticle ident)]) (summaryId article)

audioButtons :: ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
audioButtons summary maybeContent =
  case summaryId summary of
    Nothing -> []
    Just ident ->
      let maybeAudioUrl = maybeContent >>= articleAudioUrl
          maybeAudioPath = maybeContent >>= articleAudioPath
       in maybe [] (const [secondaryButton "Download audio" (GuiDownloadAudio ident)]) maybeAudioUrl
            <> maybe [] (const [secondaryButton "Open audio" (GuiOpenAudio ident)]) maybeAudioPath

articleParagraphsBlock :: [Text] -> WidgetNode Model GuiEvent
articleParagraphsBlock [] =
  emptyBlock
articleParagraphsBlock paragraphs =
  vscroll (vstack (map paragraphLabel paragraphs))
    `styleBasic` [height 280, paddingT 8, paddingB 8]

paragraphLabel :: Text -> WidgetNode Model GuiEvent
paragraphLabel paragraph =
  label paragraph
    `styleBasic` [paddingB 8, textColor mainTextColor]

articleRowsBlock :: Model -> [ArticleSummary] -> WidgetNode Model GuiEvent
articleRowsBlock _ [] =
  label "No rows loaded yet."
    `styleBasic` [textColor mutedTextColor, paddingT 16]
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
          `styleBasic` [textSize 15, textColor primaryColor, paddingT 8, paddingB 4]
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
            `styleBasic` [textSize (rowTitleSize model), textColor mainTextColor]
        , label_ (rowMeta row <> " | " <> rowKnownPct row <> " | " <> rowUploadStatus row) [ellipsis]
            `styleBasic` [textSize (rowMetaSize model), textColor mutedTextColor]
        , hstack (rowActions (currentView model) article)
            `styleBasic` [paddingT (rowActionPadding model)]
        ]
    ]
    `styleBasic`
      [ height (rowHeight model)
      , padding (rowPadding model)
      , radius 8
      , bgColor panelAltColor
      , border 1 borderColor
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

rowActions :: View -> ArticleSummary -> [WidgetNode Model GuiEvent]
rowActions BrowseView article =
  [ rowSecondaryButton "Preview" (GuiPreviewArticle article)
  , rowPrimaryButton "Fetch" (GuiFetchArticle article)
  , rowSecondaryButton "Original" (GuiOpenExternal (summaryUrl article))
  , rowSecondaryButton
      (if summaryIgnored article then "Unhide" else "Hide")
      (if summaryIgnored article then GuiUnhideBrowseArticle article else GuiHideBrowseArticle article)
  ]
    <> maybe [] (const [rowSecondaryButton "Open saved" (GuiOpenArticle article)]) (summaryId article)
rowActions _ article =
  maybe
    []
    (\ident ->
      [ rowSecondaryButton "Open" (GuiOpenArticle article)
      , rowSecondaryButton "Original" (GuiOpenExternal (summaryUrl article))
      ]
        <> uploadAction ident article
        <> [ rowSecondaryButton
               (if summaryIgnored article then "Unignore" else "Ignore")
               (GuiToggleIgnored article)
           , rowDangerButton "Delete" (GuiDeleteArticle ident)
           ])
    (summaryId article)

uploadAction :: ArticleId -> ArticleSummary -> [WidgetNode Model GuiEvent]
uploadAction ident article
  | summaryUploaded article || summaryIgnored article = []
  | otherwise = [rowPrimaryButton "Upload" (GuiUploadArticle ident)]

rowsForCurrentView :: Model -> [ArticleSummary]
rowsForCurrentView model =
  case currentView model of
    BrowseView -> visibleBrowseArticles model
    LibraryView -> libraryArticles model
    LingqView -> lingqArticles model
    ZeitLoginView -> []
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

noticeColor :: Notification -> Color
noticeColor notice =
  case notificationLevel notice of
    InfoNotice -> primaryColor
    SuccessNotice -> primaryColor
    ErrorNotice -> dangerColor

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

runFetchBatchProducer :: AppPorts IO -> Model -> [ArticleSummary] -> (GuiEvent -> IO ()) -> IO ()
runFetchBatchProducer ports model articles send =
  safeGuiProducer send $ do
    results <- traverse fetchOne (zip [1 ..] articles)
    sendProgress (length articles) "Updating the local library..."
    finalModel <-
      dispatchEvents
        ports
        model
        [ BatchFetchFinished (guiBatchFetchFailures results)
        , Notify (guiBatchFetchLevel results) (guiBatchFetchSummary results)
        , RefreshCurrentView
        ]
    send (GuiModelLoaded finalModel)
  where
    total = length articles
    sendProgress current detail =
      send (GuiProgress (Just (ProgressStatus "Fetching articles" current total detail)))
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
    filters = browseFilter model

runUploadBatchProducer :: AppPorts IO -> Model -> [ArticleSummary] -> (GuiEvent -> IO ()) -> IO ()
runUploadBatchProducer ports model articles send =
  safeGuiProducer send $ do
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
        finalModel <- dispatchEvent ports model (Notify ErrorNotice "No uploadable articles selected.")
        send (GuiModelLoaded finalModel)
      else do
        sendProgress (ProgressStatus "Uploading articles" 0 (length uploadIds) "Preparing selected articles...")
        loaded <- traverse loadOne uploadIds
        let loadFailures = [failure | Left failure <- loaded]
            uploadArticles = [article | Right article <- loaded]
        sendProgress (ProgressStatus "Uploading articles" 0 (length uploadArticles) "Uploading to LingQ...")
        uploadResults <- traverse (uploadOne config (length uploadArticles)) (zip [1 ..] uploadArticles)
        finalModel <-
          dispatchEvents
            ports
            model
            ( BatchUploadFinished (loadFailures <> guiBatchUploadFailures uploadResults)
                : guiBatchUploadResultEvents loadFailures uploadResults
                  <> [RefreshCurrentView]
            )
        send (GuiModelLoaded finalModel)
  where
    sendProgress progress =
      send (GuiProgress (Just progress))
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
      sendProgress (ProgressStatus "Uploading articles" (index - 1) total title)
      result <- tryText (uploadLessonToLingq (lingqPort ports) (uploadLanguageCode config) targetCollection titledArticle)
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
      sendProgress (ProgressStatus "Uploading articles" index total (batchUploadProgressDetail uploadResult))
      pure uploadResult

runUploadAppEvent :: AppPorts IO -> Model -> ArticleId -> IO GuiEvent
runUploadAppEvent ports model ident =
  safeGuiTask $ do
    now <- getCurrentTime
    envFallback <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
    let fallbackCollection = lingqFallbackCollection model <|> envFallback
    dispatchEvent ports model (ArticleUploadRequested (utctDay now) fallbackCollection ident)

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

guiBatchUploadResultEvents :: [(ArticleId, Text)] -> [BatchUploadResult] -> [Event]
guiBatchUploadResultEvents loadFailures results =
  [ Notify level
      ( "Batch upload: uploaded "
          <> tshow uploaded
          <> ", failed "
          <> tshow failed
          <> "."
      )
  ]
  where
    uploaded = length [() | UploadSucceeded {} <- results] + length [() | UploadSucceededUntracked {} <- results]
    failed = length loadFailures + length [() | UploadFailed {} <- results]
    level
      | failed > 0 = ErrorNotice
      | otherwise = SuccessNotice

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
  ZeitSession <$> loadConfiguredText path settingsZeitCookie "ZEIT_COOKIE"

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
  startApp initialModel (handleEvent ports) buildUI config
  where
    config =
      [ appWindowTitle "Zeit Tool Haskell"
      , appWindowState MainWindowMaximized
      , appWindowResizable True
      , appTheme darkTheme
      , appFontDef "Regular" "C:\\Windows\\Fonts\\segoeui.ttf"
      , appInitEvent GuiInit
      ]
