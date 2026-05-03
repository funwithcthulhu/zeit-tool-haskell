{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Core.KnownWords
  ( estimateKnownPct
  , importKnownWordStems
  , stemSet
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import ZeitLingq.Text.German (stemGerman, tokenizeGerman)

importKnownWordStems :: Text -> Set Text
importKnownWordStems =
  stemSet
    . concatMap tokenizeGerman
    . T.lines

stemSet :: [Text] -> Set Text
stemSet =
  Set.fromList
    . filter (not . T.null)
    . map stemGerman

estimateKnownPct :: Set Text -> Text -> Maybe Int
estimateKnownPct knownStems text
  | null stems = Nothing
  | otherwise = Just (round ((fromIntegral knownCount / fromIntegral (length stems)) * (100 :: Double)))
  where
    stems = map stemGerman (tokenizeGerman text)
    knownCount = length (filter (`Set.member` knownStems) stems)
