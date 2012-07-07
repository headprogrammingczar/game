{-# LANGUAGE CPP, OverloadedStrings #-}
module Lib.Blaze where

#include "Imports.hs"
import Lib.State

header :: String -> Html -> Html
header title body = H.docTypeHtml $ do
  H.head $ do
    H.title (H.toMarkup title)
  H.body $ do
    body

drawMap :: Grid -> Html
drawMap (Grid arr) = H.table H.! A.class_ "grid" $ do
  gridLoop arr H.tr $ \x y -> do
    let sq = arr ! (x, y)
    H.td $ H.img H.! A.src (squareImage sq)

squareImage Void = "/img/black.png"
squareImage White = "/img/white.png"

gridLoop arr row col = do
  let (x0, y0) = fst . bounds $ arr
      (xn, yn) = snd . bounds $ arr
  forM_ [x0 .. xn - 1] $ \x -> do
    row $ do
      forM_ [y0 .. yn - 1] $ \y -> do
        col x y

