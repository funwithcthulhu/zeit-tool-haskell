module ZeitLingq.App.Startup
  ( loadInitialModel
  ) where

import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.Ports (SettingsPort(..))

loadInitialModel :: Monad m => SettingsPort m -> m Model
loadInitialModel settings = do
  view <- loadCurrentView settings
  sectionId <- loadBrowseSection settings
  browseWords <- loadBrowseFilter settings
  datePrefix <- loadDatePrefixEnabled settings
  fallbackCollection <- loadLingqFallbackCollection settings
  collections <- loadSectionCollections settings
  pure
    initialModel
      { currentView = view
      , browseSectionId = sectionId
      , browseFilter = browseWords
      , datePrefixEnabled = datePrefix
      , lingqFallbackCollection = fallbackCollection
      , sectionCollections = collections
      }
