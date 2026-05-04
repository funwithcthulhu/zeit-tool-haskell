{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Gui.Theme
  ( appBgColor
  , borderColor
  , dangerBgColor
  , dangerColor
  , mainTextColor
  , monomerThemeFor
  , mutedTextColor
  , panelAltColor
  , panelBgColor
  , primaryColor
  , primaryTextColor
  , warningColor
  ) where

import Monomer (Color, Theme, darkTheme, lightTheme, rgbHex)
import ZeitLingq.App.Model (Model(..))
import ZeitLingq.Domain.Types (UiTheme(..))

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
    { paletteAppBg = rgbHex "#0d1117"
    , palettePanelBg = rgbHex "#121923"
    , palettePanelAlt = rgbHex "#182232"
    , paletteBorder = rgbHex "#2a3a4f"
    , palettePrimary = rgbHex "#5dd6a5"
    , palettePrimaryText = rgbHex "#071111"
    , paletteMainText = rgbHex "#edf3f7"
    , paletteMutedText = rgbHex "#98a6b8"
    , paletteWarning = rgbHex "#f3b047"
    , paletteDanger = rgbHex "#ff6b6b"
    , paletteDangerBg = rgbHex "#3a1f27"
    }
paletteFor LightUiTheme =
  Palette
    { paletteAppBg = rgbHex "#f6f2ea"
    , palettePanelBg = rgbHex "#fffdf7"
    , palettePanelAlt = rgbHex "#eee7dc"
    , paletteBorder = rgbHex "#d9ccba"
    , palettePrimary = rgbHex "#0f8a7a"
    , palettePrimaryText = rgbHex "#fbfffb"
    , paletteMainText = rgbHex "#17211d"
    , paletteMutedText = rgbHex "#637268"
    , paletteWarning = rgbHex "#9a5b05"
    , paletteDanger = rgbHex "#bd3b12"
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
