{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Core.Batch
  ( BatchFetchResult(..)
  , batchFetchArticles
  ) where

import Data.Text (Text)
import ZeitLingq.Domain.Article (BatchDecision(..), applyWordFilter)
import ZeitLingq.Domain.Types

data BatchFetchResult
  = BatchSaved Text ArticleId
  | BatchSkipped Text BatchDecision
  | BatchFailed Text Text
  deriving (Eq, Show)

batchFetchArticles
  :: Monad m
  => (Text -> m (Either Text Article))
  -> (Article -> m ArticleId)
  -> WordFilter
  -> [Text]
  -> m [BatchFetchResult]
batchFetchArticles fetchArticle saveArticle filters =
  traverse fetchOne
  where
    fetchOne url = do
      result <- fetchArticle url
      case result of
        Left err -> pure (BatchFailed url err)
        Right article ->
          case applyWordFilter filters article of
            KeepArticle -> BatchSaved url <$> saveArticle article
            decision -> pure (BatchSkipped url decision)
