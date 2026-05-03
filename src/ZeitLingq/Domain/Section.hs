{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Domain.Section
  ( allSections
  , lookupSection
  ) where

import Data.Text (Text)
import ZeitLingq.Domain.Types (Section(..))

allSections :: [Section]
allSections =
  [ Section "index" "Startseite" "/"
  , Section "politik" "Politik" "/politik"
  , Section "politik/deutschland" "Politik - Deutschland" "/politik/deutschland"
  , Section "politik/ausland" "Politik - Ausland" "/politik/ausland"
  , Section "wirtschaft" "Wirtschaft" "/wirtschaft"
  , Section "gesellschaft" "Gesellschaft" "/gesellschaft"
  , Section "kultur" "Kultur" "/kultur"
  , Section "wissen" "Wissen" "/wissen"
  , Section "gesundheit" "Gesundheit" "/gesundheit"
  , Section "digital" "Digital" "/digital"
  , Section "campus" "Campus" "/campus"
  , Section "arbeit" "Arbeit" "/arbeit"
  , Section "sport" "Sport" "/sport"
  , Section "entdecken" "Entdecken" "/entdecken"
  , Section "zeit-magazin" "ZEIT Magazin" "/zeit-magazin"
  , Section "mobilitat" "Mobilitat" "/mobilitaet"
  , Section "dossier" "DIE ZEIT - Dossier" "/dossier/index"
  , Section "feuilleton" "DIE ZEIT - Feuilleton" "/kultur/index"
  , Section "geschichte" "DIE ZEIT - Geschichte" "/geschichte/index"
  , Section "glauben-zweifel" "DIE ZEIT - Glauben & Zweifel" "/gesellschaft/glauben-zweifel/index"
  , Section "chancen" "DIE ZEIT - Chancen" "/gesellschaft/schule/index"
  , Section "zeit-im-osten" "DIE ZEIT - ZEIT im Osten" "/gesellschaft/zeit-im-osten/index"
  , Section "wissen-print" "DIE ZEIT - Wissen" "/wissen/index"
  , Section "die-zeit" "DIE ZEIT - Aktuelle Ausgabe" "/index"
  ]

lookupSection :: Text -> Maybe Section
lookupSection wanted = go allSections
  where
    go [] = Nothing
    go (section:rest)
      | sectionId section == wanted = Just section
      | otherwise = go rest
