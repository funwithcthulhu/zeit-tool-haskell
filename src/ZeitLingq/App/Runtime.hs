{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Runtime
  ( runCommand
  ) where

import ZeitLingq.App.Update (Command(..), Event(..))
import ZeitLingq.Domain.Types (NotificationLevel(..))
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
  where
    zeit = zeitPort ports
    library = libraryPort ports
    settings = settingsPort ports
