module Main where

import Lib.Base
import Lib.Imports
import Lib.Windows

main = runGTK $ do
  w <- mainWindow
  w `after` deleteEvent $ tryEvent $ do
    liftIO mainQuit
  widgetShowAll w

