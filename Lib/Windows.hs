module Lib.Windows where

import Lib.Imports
import Lib.Base

mainWindow :: Game Window
mainWindow = do
  window <- liftIO $ windowNew
  area <- askRef (mainMapArea . guiState)
  liftIO $ containerAdd window area
  return window

