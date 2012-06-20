module Lib.Graphics where

import Lib.Imports
import Lib.Base

-- drawing operations for objects

draw :: (Grid -> (Int, Int) -> (Int, Int) -> Render ()) -> Game ()
draw df = do
  area <- askRef (mainMapArea . guiState)
  grid <- askRef (mainMapGrid . turnState)
  coords <- askRef (viewportCenter . guiState)
  window <- liftIO $ widgetGetDrawWindow area
  dims <- liftIO $ widgetGetSize area
  liftIO $ renderWithDrawable window (df grid coords dims)

drawGrid :: Grid -> (Int, Int) -> (Int, Int) -> Render ()
drawGrid grid (cx, cy) (w, h) = do
  assoc <- liftIO $ getAssocs grid
  forM_ assoc $ \((x, y), square) -> do
    drawSquare square (x, y) (cx, cy) (w, h)

drawSquare :: Square -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Render ()
drawSquare square (sx, sy) (cx, cy) (w, h) = do
  let size = 10 -- size of the side of a square
  let (cpx, cpy) = (w `div` 2, h `div` 2) -- center pixel
  let (ccx, ccy) = (cpx - (size `div` 2), cpy - (size `div` 2)) -- top-left corner of the center square
  let (scx, scy) = (ccx + size * (sx - cx), ccy + (sy - cy)) -- top-left corner of the square we want to draw
  drawSquare' square (scx, scy)

drawSquare' :: Square -> (Int, Int) -> Render ()
drawSquare' Void (scx, scy) = return ()
drawSquare' Filled (scx, scy) = do
  return ()

