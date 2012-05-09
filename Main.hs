module Main where

import Lib.Base
import Lib.Imports
import Lib.Windows

main = runGTK $ do
  w <- liftIO mainWindow
  liftIO $ w `after` deleteEvent $ tryEvent $ do
    liftIO mainQuit
  liftIO $ widgetShowAll w
  forever gameStep

gameStep = do
  liftIO $ putStrLn "step"
  delay 1000
