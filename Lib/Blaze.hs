{-# LANGUAGE CPP, OverloadedStrings #-}
module Lib.Blaze where

#include "Imports.hs"
import Lib.State

header :: String -> Html -> Html
header title body = H.docTypeHtml $ do
  H.head $ do
    H.title (H.toMarkup title)
    H.link H.! A.rel "stylesheet" H.! A.href "/css/styles.css"
  H.body $ do
    body

drawMap :: Grid -> Html
drawMap (Grid arr) = H.table H.! A.class_ "grid" $ do
  let start = fst . bounds $ arr
      end = snd . bounds $ arr
  drawMapBounds start end (Grid arr)

drawMapBounds start end (Grid arr) = H.table H.! A.class_ "grid" $ do
  gridLoop start end arr H.tr $ \x y -> do
    let sq = arr ! (x, y)
    H.td $ do
      H.img H.! A.src (squareImage sq) H.! A.class_ "gridSquare"

squareImage Tilde = "/img/black.png"
squareImage Void = "/img/water.png"
squareImage White = "/img/clear.png"

gridLoop (x0, y0) (xn, yn) arr row col = do
  forM_ [y0 .. yn - 1] $ \y -> do
    row $ do
      forM_ [x0 .. xn - 1] $ \x -> do
        col x y

