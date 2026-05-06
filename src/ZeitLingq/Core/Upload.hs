{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Core.Upload
  ( BatchUploadConfig(..)
  , BatchUploadResult(..)
  , articleUploadFailures
  , batchUploadArticles
  , targetCollectionFor
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (Day)
import ZeitLingq.Domain.Article (lessonTitle)
import ZeitLingq.Domain.Types

data BatchUploadConfig = BatchUploadConfig
  { uploadLanguageCode :: Text
  , uploadFallbackCollection :: Maybe Text
  , uploadSectionCollections :: Map Text Text
  , uploadDatePrefixEnabled :: Bool
  , uploadDay :: Day
  } deriving (Eq, Show)

data BatchUploadResult
  = UploadSucceeded ArticleId Text LingqLesson
  | UploadSucceededUntracked Text LingqLesson
  | UploadFailed (Maybe ArticleId) Text Text
  deriving (Eq, Show)

articleUploadFailures :: [BatchUploadResult] -> [ArticleUploadFailure]
articleUploadFailures results =
  [ ArticleUploadFailure ident (Just title) reason
  | UploadFailed (Just ident) title reason <- results
  ]

batchUploadArticles
  :: Monad m
  => (Text -> Maybe Text -> Article -> m (Either Text LingqLesson))
  -> (Text -> LingqLesson -> Article -> m (Either Text LingqLesson))
  -> (ArticleId -> LingqLesson -> m ())
  -> BatchUploadConfig
  -> [Article]
  -> m [BatchUploadResult]
batchUploadArticles uploadLesson updateLesson markUploaded config =
  traverse uploadOne
  where
    uploadOne article = do
      let titledArticle =
            article
              { articleTitle =
                  lessonTitle
                    (uploadDay config)
                    (uploadDatePrefixEnabled config)
                    (articleTitle article)
              }
          targetCollection = targetCollectionFor config article
      result <-
        case articleUploadedLesson article of
          Just existingLesson -> updateLesson (uploadLanguageCode config) existingLesson titledArticle
          Nothing -> uploadLesson (uploadLanguageCode config) targetCollection titledArticle
      case result of
        Left err -> pure (UploadFailed (articleId article) (articleTitle titledArticle) err)
        Right lesson ->
          case articleId article of
            Just ident -> do
              markUploaded ident lesson
              pure (UploadSucceeded ident (articleTitle titledArticle) lesson)
            Nothing ->
              pure (UploadSucceededUntracked (articleTitle titledArticle) lesson)

targetCollectionFor :: BatchUploadConfig -> Article -> Maybe Text
targetCollectionFor BatchUploadConfig {uploadFallbackCollection, uploadSectionCollections} article =
  Map.lookup (articleSection article) uploadSectionCollections
    <|> uploadFallbackCollection

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> fallback = fallback
