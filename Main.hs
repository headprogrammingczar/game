{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP, PatternGuards #-}

#include "Lib/Imports.hs"
import Lib.State
import Lib.Blaze

import qualified System.Info
import System.Process (rawSystem)
import Control.Concurrent
import System.Console.GetOpt
import System.Environment

data Option = NoStartPage

data Options = Options {startPage :: Bool}

defaultOptions = Options {startPage = True}

modifyOptions opts NoStartPage = opts {startPage = False}

options = [Option [] ["no-start-page"] (NoArg NoStartPage) "Disable start page on startup"]

main = do
  args <- getArgs
  let (optlist, extra, errs) = getOpt RequireOrder options args
  let userOptions = foldl modifyOptions defaultOptions optlist
  when (startPage userOptions) $ openConfigPage >> return ()
  bracket (openLocalState initialGameState)
          (createCheckpointAndClose)
          (\acid -> simpleHTTP conf (game acid))

portNum = 12345
conf = nullConf {port = portNum}

game acid = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [ mzero
       , dir "css" $ serveDirectory DisableBrowsing [] "html/css"
       , dir "js" $ serveDirectory DisableBrowsing [] "html/js"
       , dir "img" $ serveDirectory DisableBrowsing [] "html/img"
       , dir "a-img" $ serveDirectory DisableBrowsing [] "html/a-img"
       , dir "start" $ startPages
       , nullDir >> homepage acid
       ]

homepage acid = do
  grid <- query' acid PeekGrid
  ok . toResponse . header "Game Map" $ do
    drawMap grid

startPages = do
  msum [ mzero
       , uriRest startPageParse
       , nullDir >> serveFile autoContentType "html/start/1.html"
       ]

startPageParse ('/':n) | [(page, "")] <- reads n = serveFile autoContentType $ "html/start/"++ show (page :: Int) ++".html"
startPageParse s = liftIO (putStrLn s) >> mzero

autoContentType = guessContentTypeM mimeTypes

openConfigPage = forkIO $ do
  case System.Info.os of
    "mingw32" -> do
      rawSystem "start" ["http://localhost:"++ show portNum ++"/start/1"]
    "linux" -> do
      rawSystem "xdg-open" ["http://localhost:"++ show portNum ++"/start/1"]
  return ()

