{-# LANGUAGE CPP #-}
module Lib.Blaze where

#include "Imports.hs"

header :: String -> Html -> Html
header title body = H.html $ do
  H.head $ do
    H.title (H.toMarkup title)
  H.body $ do
    body

