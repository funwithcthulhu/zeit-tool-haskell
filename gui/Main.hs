{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Monomer hiding (Model)
import Monomer qualified as M
import System.Environment (lookupEnv)
import ZeitLingq.App.Driver (dispatchEvent)
import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.App.Startup (loadInitialModel)
import ZeitLingq.App.Update (Event(..))
import ZeitLingq.App.ViewModel
import ZeitLingq.Domain.Section (allSections)
import ZeitLingq.Domain.Types
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
  | GuiOpenArticle ArticleSummary
  | GuiFetchArticle ArticleSummary
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
    GuiOpenArticle article ->
      [Task (runAppEvent ports model (ArticleOpened article))]
    GuiFetchArticle article ->
      [Task (runAppEvent ports model (BrowseArticleFetchRequested article))]
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
    , selectedArticleBlock model (vmSelectedArticle vm)
    , articleParagraphsBlock (vmSelectedArticleParagraphs vm)
    , articleRowsBlock (currentView model) (rowsForCurrentView model)
    ]
    `styleBasic` [padding 16]

browseControls :: Model -> WidgetNode Model GuiEvent
browseControls model =
  case currentView model of
    BrowseView ->
      vstack
        [ label "Quick sections"
            `styleBasic` [textSize 14, paddingT 8]
        , hstack (map sectionButton quickSections)
            `styleBasic` [paddingV 6]
        ]
    _ -> spacer
  where
    quickSections = take 8 allSections
    sectionButton section =
      button (sectionLabel section) (GuiSectionSelected (sectionId section))
        `styleBasic` [paddingR 4]

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
    , hstack (articleButtons (selectedArticle model))
        `styleBasic` [paddingT 6]
    ]

articleButtons :: Maybe ArticleSummary -> [WidgetNode Model GuiEvent]
articleButtons Nothing = [button "Back to library" GuiCloseArticle]
articleButtons (Just article) =
  [ button "Back to library" GuiCloseArticle
  ]
    <> maybe [] (\ident -> [button "Delete" (GuiDeleteArticle ident)]) (summaryId article)

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

articleRowsBlock :: View -> [ArticleSummary] -> WidgetNode Model GuiEvent
articleRowsBlock _ [] =
  label "No rows loaded yet."
articleRowsBlock view rows =
  scroll (vstack (map (articleRowBlock view) rows))
    `styleBasic` [height 430, paddingT 16]

articleRowBlock :: View -> ArticleSummary -> WidgetNode Model GuiEvent
articleRowBlock view article =
  hstack
    [ vstack
        [ label (rowTitle row)
        , label (rowMeta row <> " | " <> rowKnownPct row <> " | " <> rowUploadStatus row)
            `styleBasic` [textSize 12]
        ]
    , filler
    , hstack (rowActions view article)
    ]
    `styleBasic` [paddingV 8, borderB 1 (rgbHex "#2f3b48")]
  where
    row = articleRowView article

rowActions :: View -> ArticleSummary -> [WidgetNode Model GuiEvent]
rowActions BrowseView article =
  [button "Fetch" (GuiFetchArticle article)]
rowActions _ article =
  maybe
    []
    (\ident ->
      [ button "Open" (GuiOpenArticle article)
      , button "Delete" (GuiDeleteArticle ident)
      ])
    (summaryId article)

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
    dispatchEvent ports model RefreshCurrentView

runAppEvent :: AppPorts IO -> Model -> Event -> IO GuiEvent
runAppEvent ports model event =
  safeGuiTask (dispatchEvent ports model event)

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

guiLibraryPort :: LibraryPort IO
guiLibraryPort =
  LibraryPort
    { loadLibrary = \filters ->
        withLibrary dbPath $ \db -> getArticlesSqlite db filters
    , loadArticle = \ident ->
        withLibrary dbPath $ \db -> getArticleSqlite db ident
    , saveArticle = \article ->
        withLibrary dbPath $ \db -> saveArticleSqlite db article
    , deleteArticle = \ident ->
        withLibrary dbPath $ \db -> deleteArticleSqlite db ident
    , loadStats =
        withLibrary dbPath getStatsSqlite
    }

failWithShow :: Show err => err -> IO a
failWithShow = fail . show

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
