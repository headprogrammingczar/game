module Lib.Graphics where

import Lib.Imports
import Lib.Base

-- drawing operations for objects

draw :: (Grid -> (Int, Int) -> Render ()) -> Game ()
draw df = do
  area <- askRef (mainMapArea . guiState)
  grid <- askRef (mainMapGrid . turnState)
  coords <- askRef (viewportCenter . guiState)
  window <- liftIO $ widgetGetDrawWindow area
  liftIO $ renderWithDrawable window (df grid coords)

