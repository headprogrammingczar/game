{-# LANGUAGE CPP #-}
module Lib.Grid where

#include "Imports.hs"
import Lib.State
import Control.Concurrent
import System.Random
import Numeric.Noise.Perlin

type Ball = (Int, Int, Int)

gw = 199
gh = 99

generateInitialMap acid = do
  ready <- query' acid IsReady
  -- when (not ready) $ do
  forkIO $ do
    -- update' acid ClearReady
    gr <- newGrid
    update' acid (SetReady gr)
  return ()

newGrid :: IO Grid
newGrid = do
  noise <- randomIO
  let noiseGen = perlin noise 5 0.05 0.5
  balls <- forM [0..5] $ \_ -> do
    x <- randomRIO (20, gw - 20)
    y <- randomRIO (20, gh - 20)
    r <- randomRIO (10, 60)
    return (x, y, r)
  let grid = Grid $ generate ((0, 0), (gw, gh)) $ \(x,y) -> level x y balls noiseGen
  return grid

generate (low, high) f = listArray (low, high) (map f (range (low, high)))

level :: Int -> Int -> [Ball] -> Perlin -> Square
level x y balls noise | weight > 10 = Black
                      | weight > 1 = Tilde
                      | weight > 0.1 = White
                      | otherwise = Void
  where
    noiseAtCoord = noiseValue noise (fromIntegral x, fromIntegral y, 0)
    metaValue = sum . map (ballValue x y) $ balls
    edgeDist = fromIntegral $ minimum [x, y, gw - x, gh - y]
    edgeFalloff = if edgeDist > 20 then 0 else (20 - edgeDist) * 0.1
    weight = noiseAtCoord + metaValue - edgeFalloff

ballValue :: Int -> Int -> Ball -> Double
ballValue x y (bx, by, br) = fromIntegral br / (distance ** 1.7)
  where
    distance = sqrt . fromIntegral $ ((bx - x)^2 + (by - y)^2)

