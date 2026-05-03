{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Runtime
  ( runCommand
  ) where

import ZeitLingq.App.Update (Command(..), Event(..))
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
      pure [BrowseArticlesLoaded articles]
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
  where
    zeit = zeitPort ports
    library = libraryPort ports
    settings = settingsPort ports
