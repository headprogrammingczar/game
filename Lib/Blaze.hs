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
drawMap (Grid arr) = do
  let start = fst . bounds $ arr
      end = snd . bounds $ arr
  drawMapBounds start end (Grid arr)

drawMapBounds start end (Grid arr) = H.div H.! A.class_ "grid" $ do
  gridLoop start end arr $ \x y -> do
    let sq = arr ! (x, y)
    case squareImage sq of
      Nothing -> return ()
      Just img -> do
        H.img H.! A.src img H.! A.class_ "gridSquare" H.! A.style (coords x y)

coords x y = H.toValue $ "top: "++ show (y * 10) ++"px; left: "++ show (x * 10) ++"px;"

squareImage Black = Nothing
squareImage Tilde = Nothing
squareImage Void = Just "/img/water.png"
squareImage White = Nothing

gridLoop (x0, y0) (xn, yn) arr elem = do
  forM_ [y0 .. yn - 1] $ \y -> do
    forM_ [x0 .. xn - 1] $ \x -> do
      elem x y

