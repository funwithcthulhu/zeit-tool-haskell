{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Monomer
import ZeitLingq.App.Model (Model, initialModel)
import ZeitLingq.App.Update (Event(..), update)
import ZeitLingq.App.ViewModel
import ZeitLingq.Domain.Types (View)

data GuiEvent
  = GuiInit
  | GuiNavigate View
  deriving (Eq, Show)

buildUI :: WidgetEnv Model GuiEvent -> Model -> WidgetNode Model GuiEvent
buildUI _ model =
  vstack
    [ titleBlock vm
    , spacer
    , navBlock vm
    , spacer
    , statusBlock vm
    , spacer
    , contentBlock vm
    ]
    `styleBasic` [padding 24]
  where
    vm = appViewModel model

handleEvent
  :: WidgetEnv Model GuiEvent
  -> WidgetNode Model GuiEvent
  -> Model
  -> GuiEvent
  -> [AppEventResponse Model GuiEvent]
handleEvent _ _ model event =
  case event of
    GuiInit -> []
    GuiNavigate view ->
      let (nextModel, _) = update (Navigate view) model
       in [Model nextModel]

titleBlock :: AppViewModel -> WidgetNode Model GuiEvent
titleBlock vm =
  vstack
    [ label (vmTitle vm)
        `styleBasic` [textSize 28]
    , label "Haskell-native Zeit to LingQ workflow"
        `styleBasic` [textSize 14]
    ]

navBlock :: AppViewModel -> WidgetNode Model GuiEvent
navBlock vm =
  hstack (map navButton (vmNavItems vm))
    `styleBasic` [paddingV 8]

navButton :: NavItem -> WidgetNode Model GuiEvent
navButton item =
  button (navLabel item) (GuiNavigate (navView item))
    `styleBasic` [paddingH 4]

statusBlock :: AppViewModel -> WidgetNode Model GuiEvent
statusBlock vm =
  hstack (map statusLabel (vmStatusBadges vm))
    `styleBasic` [paddingV 8]

statusLabel :: StatusBadge -> WidgetNode Model GuiEvent
statusLabel badge =
  label (statusName badge <> ": " <> statusText badge)
    `styleBasic` [paddingR 16]

contentBlock :: AppViewModel -> WidgetNode Model GuiEvent
contentBlock vm =
  vstack
    [ label ("Browse section: " <> vmBrowseSection vm)
    , label ("Filter: " <> vmActiveFilter vm)
    , label (vmDatePrefix vm)
    , selectedArticleBlock (vmSelectedArticle vm)
    , articleRowsBlock (vmArticleRows vm)
    ]
    `styleBasic` [padding 16]

selectedArticleBlock :: Maybe ArticleRowView -> WidgetNode Model GuiEvent
selectedArticleBlock Nothing =
  label "No article selected."
selectedArticleBlock (Just row) =
  vstack
    [ label (rowTitle row)
        `styleBasic` [textSize 20]
    , label (rowMeta row)
    , label (rowKnownPct row <> " | " <> rowUploadStatus row)
    ]

articleRowsBlock :: [ArticleRowView] -> WidgetNode Model GuiEvent
articleRowsBlock [] =
  label "No rows loaded yet."
articleRowsBlock rows =
  vstack (map articleRowBlock rows)
    `styleBasic` [paddingT 16]

articleRowBlock :: ArticleRowView -> WidgetNode Model GuiEvent
articleRowBlock row =
  vstack
    [ label (rowTitle row)
    , label (rowMeta row <> " | " <> rowKnownPct row <> " | " <> rowUploadStatus row)
        `styleBasic` [textSize 12]
    ]
    `styleBasic` [paddingV 6]

main :: IO ()
main =
  startApp initialModel handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Zeit Tool Haskell"
      , appTheme darkTheme
      , appInitEvent GuiInit
      ]
