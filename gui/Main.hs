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
import System.Environment (lookupEnv)
import ZeitLingq.App.Driver (dispatchEvent, dispatchEvents)
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Startup (loadInitialModel)
import ZeitLingq.App.Update (Event(..))
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
  | GuiSectionSelected Text
  | GuiBrowsePreviousPage
  | GuiBrowseNextPage
  | GuiBrowseMinWordsChanged Text
  | GuiBrowseMaxWordsChanged Text
  | GuiBrowseShowHiddenChanged Bool
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
  | GuiUploadVisible [ArticleSummary]
  | GuiDownloadAudio ArticleId
  | GuiOpenAudio ArticleId
  | GuiSyncKnownWords
  | GuiKnownImportTextChanged Text
  | GuiImportKnownWords
  | GuiComputeKnownWords
  | GuiClearKnownWords
  | GuiRefreshCollections
  | GuiFallbackCollectionChanged Text
  | GuiDatePrefixChanged Bool
  | GuiSectionCollectionChanged Text Text
  | GuiLibrarySearchChanged Text
  | GuiLibraryMinWordsChanged Text
  | GuiLibraryMaxWordsChanged Text
  | GuiLibraryIncludeIgnoredChanged Bool
  | GuiLibraryOnlyIgnoredChanged Bool
  | GuiLibraryOnlyNotUploadedChanged Bool
  | GuiLibraryPreviousPage
  | GuiLibraryNextPage
  | GuiDeleteIgnoredArticles
  | GuiDeleteOldArticles Int Bool Bool
  | GuiDeleteArticle ArticleId
  | GuiCloseArticle
  | GuiClearNotice
  deriving (Eq, Show)

dbPath :: FilePath
dbPath = "zeit-tool.db"

settingsPath :: FilePath
settingsPath = "settings.json"

buildUI :: WidgetEnv Model GuiEvent -> Model -> WidgetNode Model GuiEvent
buildUI _ model =
  vstack
    [ titleBlock vm
    , spacer
    , navBlock vm
    , statusBlock vm
    , notificationBlock model
    , spacer
    , contentBlock model vm
    ]
    `styleBasic` [padding 24]
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
      [Task (runAppEvent ports model RefreshCurrentView)]
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
    GuiBrowseToggleSelection url ->
      [Task (runAppEvent ports model (BrowseSelectionToggled url))]
    GuiBrowseSelectVisible articles ->
      [Task (runAppEvent ports model (BrowseSelectionChanged (Set.fromList (map summaryUrl articles))))]
    GuiBrowseClearSelection ->
      [Task (runAppEvent ports model (BrowseSelectionChanged Set.empty))]
    GuiPreviewArticle article ->
      [Task (runAppEvent ports model (BrowseArticlePreviewRequested article))]
    GuiOpenArticle article ->
      [Task (runAppEvent ports model (ArticleOpened article))]
    GuiFetchArticle article ->
      [Task (runAppEvent ports model (BrowseArticleFetchRequested article))]
    GuiHideBrowseArticle article ->
      [Task (runAppEvent ports model (BrowseArticleHidden (summaryUrl article)))]
    GuiUnhideBrowseArticle article ->
      [Task (runAppEvent ports model (BrowseArticleUnhidden (summaryUrl article)))]
    GuiFetchSelected articles ->
      case selectedBrowseArticles model articles of
        [] -> [Task (runAppEvent ports model (Notify ErrorNotice "Select at least one article to fetch."))]
        selected -> [Task (runAppEvent ports model (BrowseBatchFetchRequested selected))]
    GuiFetchVisible articles ->
      [Task (runAppEvent ports model (BrowseBatchFetchRequested articles))]
    GuiToggleIgnored article ->
      case summaryId article of
        Just ident -> [Task (runAppEvent ports model (ArticleIgnoredChanged ident (not (summaryIgnored article))))]
        Nothing -> [Task (runAppEvent ports model (Notify ErrorNotice "Cannot ignore an unsaved article."))]
    GuiUploadArticle ident ->
      [Task (runUploadAppEvent ports model ident)]
    GuiUploadVisible articles ->
      [Task (runUploadBatchAppEvent ports model articles)]
    GuiDownloadAudio ident ->
      [Task (runAppEvent ports model (ArticleAudioDownloadRequested "audio" ident))]
    GuiOpenAudio ident ->
      [Task (runAppEvent ports model (ArticleAudioOpenRequested ident))]
    GuiSyncKnownWords ->
      [Task (runAppEvent ports model (KnownWordsSyncRequested "de"))]
    GuiKnownImportTextChanged text ->
      [Task (runAppEvent ports model (KnownWordsImportTextChanged text))]
    GuiImportKnownWords ->
      [Task (runAppEvent ports model (KnownWordsImportRequested "de" (knownImportText model) False))]
    GuiComputeKnownWords ->
      [Task (runAppEvent ports model (KnownWordsComputeRequested "de"))]
    GuiClearKnownWords ->
      [Task (runAppEvent ports model (KnownWordsClearRequested "de"))]
    GuiRefreshCollections ->
      [Task (runAppEvent ports model (LingqCollectionsRefreshRequested "de"))]
    GuiFallbackCollectionChanged collectionId ->
      [Task (runAppEvent ports model (LingqFallbackCollectionChanged collectionId))]
    GuiDatePrefixChanged enabled ->
      [Task (runAppEvent ports model (DatePrefixToggled enabled))]
    GuiSectionCollectionChanged sectionName collectionId ->
      [Task (runAppEvent ports model (SectionCollectionsChanged (updateSectionCollection sectionName collectionId (sectionCollections model))))]
    GuiLibrarySearchChanged search ->
      [Task (runAppEvent ports model (LibrarySearchChanged search))]
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
    GuiLibraryPreviousPage ->
      [Task (runAppEvent ports model (LibraryPageChanged (libraryOffset (libraryQuery model) - libraryLimit (libraryQuery model))))]
    GuiLibraryNextPage ->
      [Task (runAppEvent ports model (LibraryPageChanged (libraryOffset (libraryQuery model) + libraryLimit (libraryQuery model))))]
    GuiDeleteIgnoredArticles ->
      [Task (runAppEvent ports model LibraryDeleteIgnoredRequested)]
    GuiDeleteOldArticles days onlyUploaded onlyUnuploaded ->
      [Task (runDeleteOlderAppEvent ports model days onlyUploaded onlyUnuploaded)]
    GuiDeleteArticle ident ->
      [Task (runAppEvent ports model (ArticleDeleteRequested ident))]
    GuiCloseArticle ->
      [Task (runAppEvent ports model ArticleClosed)]
    GuiClearNotice ->
      [Task (runAppEvent ports model NotificationCleared)]

titleBlock :: AppViewModel -> WidgetNode Model GuiEvent
titleBlock vm =
  hstack
    [ vstack
        [ label (vmTitle vm)
            `styleBasic` [textSize 28]
        , label "Haskell-native Zeit to LingQ workflow"
            `styleBasic` [textSize 14]
        ]
    , filler
    , button "Refresh" GuiRefresh
    ]

navBlock :: AppViewModel -> WidgetNode Model GuiEvent
navBlock vm =
  hstack (map navButton (vmNavItems vm))
    `styleBasic` [paddingV 8]

navButton :: NavItem -> WidgetNode Model GuiEvent
navButton item =
  button (navLabel item) (GuiNavigate (navView item))
    `styleBasic`
      [ paddingH 4
      , textColor (if navActive item then cyan else white)
      ]

statusBlock :: AppViewModel -> WidgetNode Model GuiEvent
statusBlock vm =
  hstack (map statusLabel (vmStatusBadges vm))
    `styleBasic` [paddingV 8]

statusLabel :: StatusBadge -> WidgetNode Model GuiEvent
statusLabel badge =
  label (statusName badge <> ": " <> statusText badge)
    `styleBasic`
      [ paddingR 16
      , textColor (if statusConnected badge then lightGreen else orange)
      ]

notificationBlock :: Model -> WidgetNode Model GuiEvent
notificationBlock model =
  case notification model of
    Nothing -> spacer
    Just notice ->
      hstack
        [ label (noticeText notice)
            `styleBasic` [textColor (noticeColor notice)]
        , filler
        , button "Clear" GuiClearNotice
        ]
        `styleBasic` [padding 10, bgColor (rgbHex "#1d2733"), radius 8]

contentBlock :: Model -> AppViewModel -> WidgetNode Model GuiEvent
contentBlock model vm =
  vstack
    [ label ("Browse section: " <> vmBrowseSection vm)
    , label ("Filter: " <> vmActiveFilter vm)
    , label (vmDatePrefix vm)
    , browseControls model
    , libraryControls model
    , lingqControls model
    , selectedArticleBlock model (vmSelectedArticle vm)
    , articleParagraphsBlock (vmSelectedArticleParagraphs vm)
    , articleRowsBlock model (rowsForCurrentView model)
    ]
    `styleBasic` [padding 16]

browseControls :: Model -> WidgetNode Model GuiEvent
browseControls model =
  case currentView model of
    BrowseView ->
      vstack
        [ label "Quick sections"
            `styleBasic` [textSize 14, paddingT 8]
        , vstack (map (hstack . map sectionButton) sectionRows)
            `styleBasic` [paddingV 6]
        , hstack
            [ button "Previous page" GuiBrowsePreviousPage
            , label ("Page " <> tshow (browsePage model))
                `styleBasic` [paddingH 12]
            , button "Next page" GuiBrowseNextPage
            ]
        , hstack
            [ label "Fetch min"
                `styleBasic` [paddingR 6]
            , textFieldV (maybe "" tshow (minWords (browseFilter model))) GuiBrowseMinWordsChanged
                `styleBasic` [width 70]
            , label "Fetch max"
                `styleBasic` [paddingL 12, paddingR 6]
            , textFieldV (maybe "" tshow (maxWords (browseFilter model))) GuiBrowseMaxWordsChanged
                `styleBasic` [width 70]
            ]
            `styleBasic` [paddingV 6]
        , hstack
            [ button "Select all shown" (GuiBrowseSelectVisible (browseArticles model))
            , button "Deselect" GuiBrowseClearSelection
            , toggle "Show hidden" (browseShowHidden model) GuiBrowseShowHiddenChanged
            , label (tshow (Set.size (browseSelectedUrls model)) <> " selected")
                `styleBasic` [paddingL 8]
            ]
            `styleBasic` [paddingV 6]
        , hstack
            [ button ("Fetch selected (" <> tshow (Set.size (browseSelectedUrls model)) <> ")") (GuiFetchSelected (browseArticles model))
            , button ("Fetch visible (" <> T.pack (show (length (browseArticles model))) <> ")") (GuiFetchVisible (browseArticles model))
            ]
        ]
    _ -> spacer
  where
    sectionRows = chunksOf 8 allSections
    sectionButton section =
      button (sectionLabel section) (GuiSectionSelected (sectionId section))
        `styleBasic` [paddingR 4]

libraryControls :: Model -> WidgetNode Model GuiEvent
libraryControls model =
  case currentView model of
    LibraryView ->
      vstack
        [ hstack
            [ label "Search"
                `styleBasic` [paddingR 8]
            , textFieldV_ (maybe "" id (librarySearch query)) GuiLibrarySearchChanged [placeholder "Title or article text"]
                `styleBasic` [width 300]
            , label "Min"
                `styleBasic` [paddingL 12, paddingR 6]
            , textFieldV (maybe "" tshow (minWords (libraryWordFilter query))) GuiLibraryMinWordsChanged
                `styleBasic` [width 70]
            , label "Max"
                `styleBasic` [paddingL 12, paddingR 6]
            , textFieldV (maybe "" tshow (maxWords (libraryWordFilter query))) GuiLibraryMaxWordsChanged
                `styleBasic` [width 70]
            ]
        , hstack
            [ toggle "Show ignored" (libraryIncludeIgnored query) GuiLibraryIncludeIgnoredChanged
            , toggle "Only ignored" (libraryOnlyIgnored query) GuiLibraryOnlyIgnoredChanged
            , toggle "Only not uploaded" (libraryOnlyNotUploaded query) GuiLibraryOnlyNotUploadedChanged
            ]
            `styleBasic` [paddingV 8]
        , hstack
            [ button "Previous" GuiLibraryPreviousPage
            , label (libraryPageLabel model)
                `styleBasic` [paddingH 12]
            , button "Next" GuiLibraryNextPage
            ]
        , hstack
            [ button "Delete ignored" GuiDeleteIgnoredArticles
            , button "Delete old 30d" (GuiDeleteOldArticles 30 False False)
            , button "Delete old uploaded 30d" (GuiDeleteOldArticles 30 True False)
            ]
            `styleBasic` [paddingT 8]
        ]
        `styleBasic` [paddingV 8]
    _ -> spacer
  where
    query = libraryQuery model

toggle :: Text -> Bool -> (Bool -> GuiEvent) -> WidgetNode Model GuiEvent
toggle text value handler =
  hstack
    [ checkboxV value handler
    , label text
        `styleBasic` [paddingL 4, paddingR 16]
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
        [ hstack
            [ button ("Upload visible (" <> T.pack (show (length uploadable)) <> ")") (GuiUploadVisible (lingqArticles model))
            , button "Sync known words" GuiSyncKnownWords
            , button "Refresh %" GuiComputeKnownWords
            , button "Refresh collections" GuiRefreshCollections
            , toggle "Auto-date lesson titles" (datePrefixEnabled model) GuiDatePrefixChanged
            ]
        , label ("Known German stems: " <> tshow (knownStemTotal model))
            `styleBasic` [paddingT 8]
        , hstack
            [ label "Fallback collection"
                `styleBasic` [paddingR 8]
            , textFieldV_
                (maybe "" id (lingqFallbackCollection model))
                GuiFallbackCollectionChanged
                [placeholder "collection id or blank"]
                `styleBasic` [width 260]
            ]
            `styleBasic` [paddingT 8]
        , collectionList model
        , label "Import known words"
            `styleBasic` [paddingT 8]
        , textAreaV_
            (knownImportText model)
            GuiKnownImportTextChanged
            [maxLines 6]
            `styleBasic` [height 120]
        , hstack
            [ button "Import pasted words" GuiImportKnownWords
            , button "Clear known words" GuiClearKnownWords
            ]
            `styleBasic` [paddingT 6]
        , label "Per-section LingQ collection ids"
            `styleBasic` [paddingT 8]
        , vstack (map collectionRow allSections)
        ]
        `styleBasic` [paddingV 8]
    _ -> spacer
  where
    uploadable =
      [ article
      | article <- lingqArticles model
      , not (summaryUploaded article)
      , not (summaryIgnored article)
      ]
    collectionRow section =
      hstack
        [ label (sectionLabel section)
            `styleBasic` [width 110]
        , textFieldV_
            (Map.findWithDefault "" (sectionLabel section) (sectionCollections model))
            (GuiSectionCollectionChanged (sectionLabel section))
            [placeholder "collection id"]
            `styleBasic` [width 220]
        ]
        `styleBasic` [paddingB 4]

collectionList :: Model -> WidgetNode Model GuiEvent
collectionList model
  | null (lingqCollections model) =
      label "No LingQ collections loaded yet. Connect with LINGQ_API_KEY, then refresh collections."
        `styleBasic` [textSize 12, paddingT 6]
  | otherwise =
      vstack
        [ label "Fetched collections (click to use as fallback)"
            `styleBasic` [textSize 12, paddingT 6]
        , vstack (map (hstack . map collectionButton) (chunksOf 3 (lingqCollections model)))
        ]
  where
    collectionButton collection =
      button
        (collectionTitle collection <> " (" <> tshow (collectionLessonsCount collection) <> ")")
        (GuiFallbackCollectionChanged (collectionId collection))
        `styleBasic` [paddingR 4, paddingB 4]

selectedArticleBlock :: Model -> Maybe ArticleRowView -> WidgetNode Model GuiEvent
selectedArticleBlock model Nothing =
  case currentView model of
    ArticleView -> label "No article selected."
    _ -> spacer
selectedArticleBlock model (Just row) =
  vstack
    [ label (rowTitle row)
        `styleBasic` [textSize 20, paddingT 12]
    , label (rowMeta row)
    , label (rowKnownPct row <> " | " <> rowUploadStatus row)
    , hstack (articleButtons (selectedArticle model) (selectedArticleContent model))
        `styleBasic` [paddingT 6]
    ]

articleButtons :: Maybe ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
articleButtons Nothing _ = [button "Back to library" GuiCloseArticle]
articleButtons (Just article) maybeContent =
  [ button "Back to library" GuiCloseArticle
  ]
    <> maybe [] (\ident -> uploadAction ident article) (summaryId article)
    <> audioButtons article maybeContent
    <> maybe [] (\ident -> [button "Delete" (GuiDeleteArticle ident)]) (summaryId article)

audioButtons :: ArticleSummary -> Maybe Article -> [WidgetNode Model GuiEvent]
audioButtons summary maybeContent =
  case summaryId summary of
    Nothing -> []
    Just ident ->
      let maybeAudioUrl = maybeContent >>= articleAudioUrl
          maybeAudioPath = maybeContent >>= articleAudioPath
       in maybe [] (const [button "Download audio" (GuiDownloadAudio ident)]) maybeAudioUrl
            <> maybe [] (const [button "Open audio" (GuiOpenAudio ident)]) maybeAudioPath

articleParagraphsBlock :: [Text] -> WidgetNode Model GuiEvent
articleParagraphsBlock [] =
  spacer
articleParagraphsBlock paragraphs =
  scroll (vstack (map paragraphLabel paragraphs))
    `styleBasic` [height 340, paddingT 12]

paragraphLabel :: Text -> WidgetNode Model GuiEvent
paragraphLabel paragraph =
  label paragraph
    `styleBasic` [paddingB 8]

articleRowsBlock :: Model -> [ArticleSummary] -> WidgetNode Model GuiEvent
articleRowsBlock _ [] =
  label "No rows loaded yet."
articleRowsBlock model rows =
  scroll (vstack (map (articleRowBlock model) rows))
    `styleBasic` [height 430, paddingT 16]

articleRowBlock :: Model -> ArticleSummary -> WidgetNode Model GuiEvent
articleRowBlock model article =
  hstack
    [ browseSelectionCheckbox model article
    , vstack
        [ label (rowTitle row)
        , label (rowMeta row <> " | " <> rowKnownPct row <> " | " <> rowUploadStatus row)
            `styleBasic` [textSize 12]
        ]
    , filler
    , hstack (rowActions (currentView model) article)
    ]
    `styleBasic` [paddingV 8, borderB 1 (rgbHex "#2f3b48")]
  where
    row = articleRowView article

browseSelectionCheckbox :: Model -> ArticleSummary -> WidgetNode Model GuiEvent
browseSelectionCheckbox model article =
  case currentView model of
    BrowseView ->
      checkboxV
        (Set.member (summaryUrl article) (browseSelectedUrls model))
        (const (GuiBrowseToggleSelection (summaryUrl article)))
        `styleBasic` [paddingR 8]
    _ -> spacer

rowActions :: View -> ArticleSummary -> [WidgetNode Model GuiEvent]
rowActions BrowseView article =
  [ button "Preview" (GuiPreviewArticle article)
  , button "Fetch" (GuiFetchArticle article)
  , button
      (if summaryIgnored article then "Unhide" else "Hide")
      (if summaryIgnored article then GuiUnhideBrowseArticle article else GuiHideBrowseArticle article)
  ]
rowActions _ article =
  maybe
    []
    (\ident ->
      [ button "Open" (GuiOpenArticle article)
      ]
        <> uploadAction ident article
        <> [ button
               (if summaryIgnored article then "Unignore" else "Ignore")
               (GuiToggleIgnored article)
           , button "Delete" (GuiDeleteArticle ident)
           ])
    (summaryId article)

uploadAction :: ArticleId -> ArticleSummary -> [WidgetNode Model GuiEvent]
uploadAction ident article
  | summaryUploaded article || summaryIgnored article = []
  | otherwise = [button "Upload" (GuiUploadArticle ident)]

rowsForCurrentView :: Model -> [ArticleSummary]
rowsForCurrentView model =
  case currentView model of
    BrowseView -> browseArticles model
    LibraryView -> libraryArticles model
    LingqView -> lingqArticles model
    ZeitLoginView -> []
    ArticleView -> maybe [] (: []) (selectedArticle model)

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
    InfoNotice -> cyan
    SuccessNotice -> lightGreen
    ErrorNotice -> red

loadGuiInitialModel :: AppPorts IO -> IO GuiEvent
loadGuiInitialModel ports =
  safeGuiTask $ do
    model <- loadInitialModel (settingsPort ports)
    zeit <- loginToZeit (zeitPort ports)
    lingq <- loginToLingq (lingqPort ports) "" ""
    dispatchEvents ports model [ZeitStatusChanged zeit, LingqStatusChanged lingq, RefreshCurrentView]

runAppEvent :: AppPorts IO -> Model -> Event -> IO GuiEvent
runAppEvent ports model event =
  safeGuiTask (dispatchEvent ports model event)

runUploadAppEvent :: AppPorts IO -> Model -> ArticleId -> IO GuiEvent
runUploadAppEvent ports model ident =
  safeGuiTask $ do
    now <- getCurrentTime
    envFallback <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
    let fallbackCollection = lingqFallbackCollection model <|> envFallback
    dispatchEvent ports model (ArticleUploadRequested (utctDay now) fallbackCollection ident)

runUploadBatchAppEvent :: AppPorts IO -> Model -> [ArticleSummary] -> IO GuiEvent
runUploadBatchAppEvent ports model articles =
  safeGuiTask $ do
    now <- getCurrentTime
    envFallback <- fmap T.pack <$> lookupEnv "LINGQ_COLLECTION_ID"
    let fallbackCollection = lingqFallbackCollection model <|> envFallback
    dispatchEvent ports model (LingqBatchUploadRequested (utctDay now) fallbackCollection articles)

runDeleteOlderAppEvent :: AppPorts IO -> Model -> Int -> Bool -> Bool -> IO GuiEvent
runDeleteOlderAppEvent ports model days onlyUploaded onlyUnuploaded =
  safeGuiTask $ do
    now <- getCurrentTime
    let cutoff = addUTCTime (negate (fromIntegral days * 86400)) now
    dispatchEvent ports model (LibraryDeleteOlderRequested cutoff onlyUploaded onlyUnuploaded)

safeGuiTask :: IO Model -> IO GuiEvent
safeGuiTask action = do
  result <- try action
  pure $
    case result of
      Right model -> GuiModelLoaded model
      Left err -> GuiFailed (T.pack (displayException (err :: SomeException)))

makePorts :: IO (AppPorts IO)
makePorts = do
  maybeCookie <- lookupEnv "ZEIT_COOKIE"
  maybeLingqKey <- lookupEnv "LINGQ_API_KEY"
  let session = ZeitSession (maybe "" T.pack maybeCookie)
      lingqToken = LingqToken . T.pack <$> maybeLingqKey
  pure
    AppPorts
      { zeitPort = guiZeitPort session
      , lingqPort = guiLingqPort lingqToken
      , audioPort = guiAudioPort
      , libraryPort = guiLibraryPort
      , settingsPort = jsonSettingsPort settingsPath
      }

guiZeitPort :: ZeitSession -> ZeitPort IO
guiZeitPort session =
  ZeitPort
    { fetchSections = pure allSections
    , fetchArticleList = \sectionIdent page ->
        either failWithShow pure =<< fetchArticleListZeit session sectionIdent page
    , fetchArticleContent = \url ->
        either failWithShow pure =<< fetchArticleContentZeit session url
    , loginToZeit =
        pure
          AuthStatus
            { authLoggedIn = not (T.null (zeitCookies session))
            , authLabel =
                if T.null (zeitCookies session)
                  then Just "set ZEIT_COOKIE"
                  else Just "cookie session"
            }
    , logoutFromZeit = pure ()
    }

guiLingqPort :: Maybe LingqToken -> LingqPort IO
guiLingqPort maybeToken =
  LingqPort
    { loginToLingq = \_ _ -> pure lingqStatusFromEnv
    , logoutFromLingq = pure ()
    , uploadLessonToLingq = \languageCode collectionId article -> do
        token <- maybe (fail "Set LINGQ_API_KEY before uploading to LingQ.") pure maybeToken
        either failWithShow pure =<< uploadLessonLingq token languageCode collectionId article
    , fetchCollections = \languageCode -> do
        token <- maybe (fail "Set LINGQ_API_KEY before loading LingQ collections.") pure maybeToken
        either failWithShow pure =<< fetchCollectionsLingq token languageCode
    , fetchKnownWords = \languageCode -> do
        token <- maybe (fail "Set LINGQ_API_KEY before syncing known words.") pure maybeToken
        either failWithShow pure =<< fetchKnownWordsLingq token languageCode
    }
  where
    lingqStatusFromEnv =
      AuthStatus
        { authLoggedIn = maybe False (const True) maybeToken
        , authLabel = maybe (Just "set LINGQ_API_KEY") (const (Just "API key")) maybeToken
        }

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf size values
  | size <= 0 = [values]
chunksOf _ [] = []
chunksOf size values =
  let (chunk, rest) = splitAt size values
   in chunk : chunksOf size rest

main :: IO ()
main = do
  ports <- makePorts
  startApp initialModel (handleEvent ports) buildUI config
  where
    config =
      [ appWindowTitle "Zeit Tool Haskell"
      , appWindowState (MainWindowNormal (1180, 760))
      , appTheme darkTheme
      , appFontDef "Regular" "C:\\Windows\\Fonts\\segoeui.ttf"
      , appInitEvent GuiInit
      ]
