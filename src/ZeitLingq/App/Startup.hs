module ZeitLingq.App.Startup
  ( loadInitialModel
  ) where

import ZeitLingq.App.Model (Model(..), initialModel)
import ZeitLingq.Ports (SettingsPort(..))

loadInitialModel :: Monad m => SettingsPort m -> m Model
loadInitialModel settings = do
  view <- loadCurrentView settings
  sectionId <- loadBrowseSection settings
  datePrefix <- loadDatePrefixEnabled settings
  collections <- loadSectionCollections settings
  pure
    initialModel
      { currentView = view
      , browseSectionId = sectionId
      , datePrefixEnabled = datePrefix
      , sectionCollections = collections
      }
