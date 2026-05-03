{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Runtime
  ( runCommand
  ) where

import ZeitLingq.App.Update (Command(..))
import ZeitLingq.Ports

runCommand :: Applicative m => AppPorts m -> Command -> m ()
runCommand ports command =
  case command of
    PersistCurrentView view ->
      saveCurrentView settings view
    PersistBrowseSection sectionId ->
      saveBrowseSection settings sectionId
    PersistDatePrefix enabled ->
      saveDatePrefixEnabled settings enabled
    PersistSectionCollections mappings ->
      saveSectionCollections settings mappings
    RefreshBrowse ->
      pure ()
    RefreshLibrary ->
      pure ()
    RefreshLingqLibrary ->
      pure ()
  where
    settings = settingsPort ports
