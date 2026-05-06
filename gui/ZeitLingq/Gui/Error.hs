{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Gui.Error (
  fetchFailuresNeedZeitLogin,
  friendlyFailureMessage,
  zeitAuthGuidance,
) where

import Data.Text (Text)
import qualified Data.Text as T
import ZeitLingq.Domain.Types (ArticleFetchFailure (..))

friendlyFailureMessage :: Text -> Text
friendlyFailureMessage message
  | isZeitAuthFailureText message = message <> " " <> zeitAuthGuidance
  | otherwise = message

fetchFailuresNeedZeitLogin :: [ArticleFetchFailure] -> Bool
fetchFailuresNeedZeitLogin =
  any (isZeitAuthFailureText . articleFetchFailureReason)

isZeitAuthFailureText :: Text -> Bool
isZeitAuthFailureText message =
  any
    (`T.isInfixOf` lower)
    [ "session expired"
    , "paywall"
    , "behind a paywall"
    , "einloggen"
    , "zeit request failed"
    ]
 where
  lower = T.toLower message

zeitAuthGuidance :: Text
zeitAuthGuidance =
  "Open the Zeit tab and use Browser login & import so requests reuse your real Edge/Chrome session."
