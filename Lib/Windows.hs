module Lib.Windows where

import Lib.Imports
import Lib.Base

mainWindow = do
  window <- windowNew
  (img, canvas) <- newCanvas 600 400
  area <- scrollArea img
  containerAdd window area
  return window

