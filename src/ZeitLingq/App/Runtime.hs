{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Runtime
  ( runCommand
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import ZeitLingq.App.Update (Command(..), Event(..))
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.Core.Batch (BatchFetchResult(..), batchFetchArticles)
import ZeitLingq.Core.Browse (hideIgnoredSummaries)
import ZeitLingq.Core.Upload (BatchUploadResult(..), batchUploadArticles)
import ZeitLingq.Domain.Article (wordCount)
import ZeitLingq.Domain.Types
import ZeitLingq.Ports

runCommand :: Monad m => AppPorts m -> Command -> m [Event]
runCommand ports command =
  case command of
    PersistCurrentView view ->
      [] <$ saveCurrentView settings view
    PersistBrowseSection sectionId ->
      [] <$ saveBrowseSection settings sectionId
    PersistDatePrefix enabled ->
      [] <$ saveDatePrefixEnabled settings enabled
    PersistSectionCollections mappings ->
      [] <$ saveSectionCollections settings mappings
    RefreshBrowse sectionId page -> do
      articles <- fetchArticleList zeit sectionId page
      ignoredUrls <- Set.fromList <$> loadIgnoredUrls library
      pure [BrowseArticlesLoaded (hideIgnoredSummaries ignoredUrls articles)]
    RefreshLibrary filters -> do
      articles <- loadLibrary library filters
      pure [LibraryArticlesLoaded articles]
    RefreshLingqLibrary filters -> do
      articles <- loadLibrary library filters
      pure [LingqArticlesLoaded articles]
    LoadArticle ident -> do
      article <- loadArticle library ident
      pure
        [ maybe
            (Notify ErrorNotice "Article not found.")
            ArticleContentLoaded
            article
        ]
    FetchAndSaveArticle summary -> do
      article <- fetchArticleContent zeit (summaryUrl summary)
      savedId <- saveArticle library article
      pure
        [ Notify SuccessNotice ("Saved article " <> summaryTitle summary <> ".")
        , LibraryArticlesLoaded
            [ summary
                { summaryId = Just savedId
                , summarySection = articleSection article
                , summaryWordCount = wordCount article
                }
            ]
        , RefreshCurrentView
        ]
    DeleteSavedArticle ident -> do
      deleteArticle library ident
      pure
        [ Notify SuccessNotice "Deleted article."
        , ArticleClosed
        ]
    SetArticleIgnored ident ignored -> do
      setArticleIgnored library ident ignored
      pure
        [ Notify SuccessNotice (if ignored then "Article ignored." else "Article unignored.")
        , RefreshCurrentView
        ]
    UploadSavedArticle day fallbackCollection sectionCollections datePrefix ident -> do
      maybeArticle <- loadArticle library ident
      case maybeArticle of
        Nothing ->
          pure [Notify ErrorNotice "Article not found."]
        Just article -> do
          results <-
            batchUploadArticles
              (\languageCode collectionId titledArticle ->
                Right <$> uploadLessonToLingq lingq languageCode collectionId titledArticle)
              (markArticleUploaded library)
              (uploadConfigFromPreferences day fallbackCollection datePrefix sectionCollections)
              [article]
          pure (concatMap uploadResultEvents results <> [RefreshCurrentView])
    SetBrowseUrlIgnored url -> do
      ignoreArticleUrl library url
      pure
        [ Notify SuccessNotice "Article hidden from browse."
        , RefreshCurrentView
        ]
    FetchAndSaveArticles filters summaries -> do
      results <-
        batchFetchArticles
          (\url -> Right <$> fetchArticleContent zeit url)
          (saveArticle library)
          filters
          (map summaryUrl summaries)
      pure
        [ Notify SuccessNotice (batchFetchSummary results)
        , RefreshCurrentView
        ]
  where
    zeit = zeitPort ports
    lingq = lingqPort ports
    library = libraryPort ports
    settings = settingsPort ports

uploadResultEvents :: BatchUploadResult -> [Event]
uploadResultEvents result =
  case result of
    UploadSucceeded _ title _ ->
      [Notify SuccessNotice ("Uploaded " <> title <> " to LingQ.")]
    UploadSucceededUntracked title _ ->
      [Notify SuccessNotice ("Uploaded " <> title <> " to LingQ.")]
    UploadFailed _ title err ->
      [Notify ErrorNotice ("Could not upload " <> title <> ": " <> err)]

batchFetchSummary :: [BatchFetchResult] -> Text
batchFetchSummary results =
  "Batch fetch: saved "
    <> tshow saved
    <> ", skipped "
    <> tshow skipped
    <> ", failed "
    <> tshow failed
    <> "."
  where
    saved = length [() | BatchSaved {} <- results]
    skipped = length [() | BatchSkipped {} <- results]
    failed = length [() | BatchFailed {} <- results]

tshow :: Show a => a -> Text
tshow = T.pack . show
