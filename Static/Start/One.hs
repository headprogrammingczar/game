{-# LANGUAGE CPP, OverloadedStrings #-}
#include "../../Lib/Imports.hs"
import Text.Blaze.Html.Renderer.String
import Lib.Blaze

main = putStrLn . renderHtml $ do
  header "Configure" $ do
    p ""

