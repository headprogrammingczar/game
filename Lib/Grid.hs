{-# LANGUAGE CPP #-}
module Lib.Grid where

#include "Imports.hs"
import Lib.State
import Control.Concurrent

generateInitialMap acid = do
  ready <- query' acid IsReady
  when (not ready) $ do
    forkIO $ do
      update' acid ClearReady
    return ()

