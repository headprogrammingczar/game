{-# LANGUAGE CPP #-}
module Lib.Grid where

#include "Imports.hs"
import Lib.State
import Control.Concurrent
import System.Random

generateInitialMap acid = do
  ready <- query' acid IsReady
  when (not ready) $ do
    forkIO $ do
      update' acid ClearReady
      gr <- newGrid
      update' acid (SetReady gr)
    return ()

newGrid :: IO Grid
newGrid = do
  rs <- randoms <$> getStdGen
  let rs' = map convert rs
  let grid = Grid $ listArray ((0, 0), (299, 199)) rs'
  return grid

convert False = Void
convert True = White

voidGrid = Grid $ listArray ((0, 0), (299, 199)) (repeat Void)

