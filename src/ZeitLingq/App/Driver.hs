module ZeitLingq.App.Driver (
  dispatchEvent,
  dispatchEvents,
) where

import Control.Monad (foldM)
import ZeitLingq.App.Model (Model)
import ZeitLingq.App.Runtime (runCommand)
import ZeitLingq.App.Update (Event, update)
import ZeitLingq.Ports (AppPorts)

dispatchEvent :: (Monad m) => AppPorts m -> Model -> Event -> m Model
dispatchEvent ports model event = do
  let (nextModel, commands) = update event model
  followUpEvents <- concat <$> traverse (runCommand ports) commands
  dispatchEvents ports nextModel followUpEvents

dispatchEvents :: (Monad m) => AppPorts m -> Model -> [Event] -> m Model
dispatchEvents ports =
  foldM (dispatchEvent ports)
