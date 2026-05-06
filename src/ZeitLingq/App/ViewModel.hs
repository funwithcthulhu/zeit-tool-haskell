{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.ViewModel (
  AppViewModel (..),
  ArticleRowView (..),
  NavItem (..),
  StatusBadge (..),
  appViewModel,
  articleRowView,
  viewLabel,
  wordFilterLabel,
) where

import Data.Text (Text)
import Data.Text qualified as T
import ZeitLingq.App.Model (Model (..))
import ZeitLingq.Domain.Types

data AppViewModel = AppViewModel
  { vmTitle :: Text
  , vmNavItems :: [NavItem]
  , vmStatusBadges :: [StatusBadge]
  , vmBrowseSection :: Text
  , vmActiveFilter :: Text
  , vmDatePrefix :: Text
  , vmSelectedArticle :: Maybe ArticleRowView
  , vmSelectedArticleParagraphs :: [Text]
  , vmArticleRows :: [ArticleRowView]
  }
  deriving (Eq, Show)

data NavItem = NavItem
  { navView :: View
  , navLabel :: Text
  , navActive :: Bool
  }
  deriving (Eq, Show)

data StatusBadge = StatusBadge
  { statusName :: Text
  , statusText :: Text
  , statusConnected :: Bool
  }
  deriving (Eq, Show)

data ArticleRowView = ArticleRowView
  { rowId :: Maybe ArticleId
  , rowTitle :: Text
  , rowMeta :: Text
  , rowKnownPct :: Text
  , rowUploadStatus :: Text
  }
  deriving (Eq, Show)

appViewModel :: Model -> AppViewModel
appViewModel model =
  AppViewModel
    { vmTitle = titleFor model
    , vmNavItems = map (navFor (currentView model)) mainViews
    , vmStatusBadges =
        [ statusBadge "Zeit" (zeitStatus model)
        , statusBadge "LingQ" (lingqStatus model)
        ]
    , vmBrowseSection = browseSectionId model
    , vmActiveFilter = filterForCurrentView model
    , vmDatePrefix =
        if datePrefixEnabled model
          then "Date prefix: on"
          else "Date prefix: off"
    , vmSelectedArticle = articleRowView <$> selectedArticle model
    , vmSelectedArticleParagraphs =
        maybe [] articleParagraphs (selectedArticleContent model)
    , vmArticleRows = map articleRowView (rowsForCurrentView model)
    }

articleRowView :: ArticleSummary -> ArticleRowView
articleRowView article =
  ArticleRowView
    { rowId = summaryId article
    , rowTitle = summaryTitle article
    , rowMeta =
        summarySection article
          <> " | "
          <> tshow (summaryWordCount article)
          <> " words"
    , rowKnownPct = knownPctLabel (summaryKnownPct article)
    , rowUploadStatus =
        case (summaryIgnored article, summaryUploaded article) of
          (True, _) -> "ignored"
          (_, True) -> "uploaded"
          _ -> "not uploaded"
    }

viewLabel :: View -> Text
viewLabel BrowseView = "Browse"
viewLabel LibraryView = "Library"
viewLabel LingqView = "LingQ"
viewLabel ZeitLoginView = "Zeit"
viewLabel DiagnosticsView = "Diagnostics"
viewLabel ArticleView = "Article"

wordFilterLabel :: WordFilter -> Text
wordFilterLabel (WordFilter Nothing Nothing) = "any length"
wordFilterLabel (WordFilter (Just minValue) Nothing) = "at least " <> tshow minValue <> " words"
wordFilterLabel (WordFilter Nothing (Just maxValue)) = "up to " <> tshow maxValue <> " words"
wordFilterLabel (WordFilter (Just minValue) (Just maxValue)) =
  tshow minValue <> "-" <> tshow maxValue <> " words"

mainViews :: [View]
mainViews = [BrowseView, LibraryView, LingqView, ZeitLoginView, DiagnosticsView]

navFor :: View -> View -> NavItem
navFor active view =
  NavItem
    { navView = view
    , navLabel = viewLabel view
    , navActive = active == view
    }

titleFor :: Model -> Text
titleFor model =
  case currentView model of
    BrowseView -> "Browse Die Zeit"
    LibraryView -> "Saved Articles"
    LingqView -> "LingQ Uploads"
    ZeitLoginView -> "Zeit Session"
    DiagnosticsView -> "Diagnostics"
    ArticleView ->
      maybe "Article" summaryTitle (selectedArticle model)

filterForCurrentView :: Model -> Text
filterForCurrentView model =
  case currentView model of
    BrowseView -> wordFilterLabel (browseFilter model)
    LibraryView -> wordFilterLabel (libraryFilter model)
    LingqView -> wordFilterLabel (lingqFilter model)
    ZeitLoginView -> "authentication"
    DiagnosticsView -> "jobs and logs"
    ArticleView -> "selected article"

rowsForCurrentView :: Model -> [ArticleSummary]
rowsForCurrentView model =
  case currentView model of
    BrowseView -> browseArticles model
    LibraryView -> libraryArticles model
    LingqView -> lingqArticles model
    ZeitLoginView -> []
    DiagnosticsView -> []
    ArticleView -> maybe [] (: []) (selectedArticle model)

statusBadge :: Text -> AuthStatus -> StatusBadge
statusBadge name status =
  StatusBadge
    { statusName = name
    , statusText =
        case authLabel status of
          Just label | not (T.null label) -> label
          _ ->
            if authLoggedIn status
              then "connected"
              else "disconnected"
    , statusConnected = authLoggedIn status
    }

knownPctLabel :: Maybe Int -> Text
knownPctLabel Nothing = "known: -"
knownPctLabel (Just pct) = "known: " <> tshow pct <> "%"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
