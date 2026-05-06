{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.UploadConfig (
  uploadConfigFromPreferences,
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (Day)
import ZeitLingq.Core.Upload (BatchUploadConfig (..))

uploadConfigFromPreferences ::
  Day -> Text -> Maybe Text -> Bool -> Map Text Text -> BatchUploadConfig
uploadConfigFromPreferences day languageCode fallbackCollection datePrefixEnabled sectionCollections =
  BatchUploadConfig
    { uploadLanguageCode = languageCode
    , uploadFallbackCollection = fallbackCollection
    , uploadSectionCollections = sectionCollections
    , uploadDatePrefixEnabled = datePrefixEnabled
    , uploadDay = day
    }
