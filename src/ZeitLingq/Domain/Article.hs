{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Domain.Article
  ( BatchDecision(..)
  , applyWordFilter
  , articleBodyText
  , articleSummary
  , composeCleanText
  , lessonTitle
  , wordCount
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import ZeitLingq.Domain.Types

data BatchDecision
  = KeepArticle
  | SkipBelowMinimum Int Int
  | SkipAboveMaximum Int Int
  deriving (Eq, Show)

articleBodyText :: Article -> Text
articleBodyText = T.intercalate "\n\n" . articleParagraphs

wordCount :: Article -> Int
wordCount =
  length
    . filter (not . T.null)
    . T.words
    . articleBodyText

composeCleanText :: Article -> Text
composeCleanText article =
  T.intercalate
    ""
    ( filter
        (not . T.null)
        [ articleTitle article
        , prefixedLine (articleSubtitle article)
        , maybe "" (\author -> "\nVon " <> author) nonEmptyAuthor
        , maybe "" (\dateValue -> "\n" <> dateValue) (articleDate article)
        , "\n\n---\n\n"
        , articleBodyText article
        ]
    )
  where
    nonEmptyAuthor =
      let author = articleAuthor article
       in if T.null author then Nothing else Just author

    prefixedLine value
      | T.null value = ""
      | otherwise = "\n" <> value

applyWordFilter :: WordFilter -> Article -> BatchDecision
applyWordFilter filters article =
  case (minWords filters, maxWords filters) of
    (Just minValue, _) | totalWords < minValue -> SkipBelowMinimum totalWords minValue
    (_, Just maxValue) | totalWords > maxValue -> SkipAboveMaximum totalWords maxValue
    _ -> KeepArticle
  where
    totalWords = wordCount article

lessonTitle :: Day -> Bool -> Text -> Text
lessonTitle today shouldPrefix rawTitle
  | not shouldPrefix = title
  | alreadyPrefixed = title
  | otherwise = stamp <> " - " <> title
  where
    title =
      if T.null (T.strip rawTitle)
        then "Untitled"
        else T.strip rawTitle
    stamp = T.pack (formatTime defaultTimeLocale "%F" today)
    alreadyPrefixed =
      case T.splitOn " " title of
        firstToken : _
          | T.length firstToken == 10 ->
              T.all (\ch -> ch == '-' || ('0' <= ch && ch <= '9')) firstToken
        _ -> False

articleSummary :: Article -> ArticleSummary
articleSummary article =
  ArticleSummary
    { summaryId = articleId article
    , summaryUrl = articleUrl article
    , summaryTitle = articleTitle article
    , summarySection = articleSection article
    , summaryWordCount = wordCount article
    , summaryIgnored = articleIgnored article
    , summaryUploaded = maybe False (const True) (articleUploadedLesson article)
    , summaryKnownPct = articleKnownPct article
    }
