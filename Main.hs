{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP, PatternGuards #-}

#include "Lib/Imports.hs"
import Lib.State
import Lib.Blaze
import Lib.Grid

import qualified System.Info
import System.Process (rawSystem)
import Control.Concurrent
import System.Console.GetOpt
import System.Environment

data Option = NoStartPage | PortNum Int

data Options = Options {startPage :: Bool, portNum :: Int}

defaultOptions = Options {startPage = True, portNum = 12345}

modifyOptions opts NoStartPage = opts {startPage = False}
modifyOptions opts (PortNum p) = opts {portNum = p}

options = [Option [] ["no-start-page"] (NoArg NoStartPage) "Disable start page on startup",
           Option ['p'] ["port"] (ReqArg (PortNum . read) "Port Number") "Set port number"]

main = do
  args <- getArgs
  let (optlist, extra, errs) = getOpt RequireOrder options args
  let userOptions = foldl modifyOptions defaultOptions optlist
  when (startPage userOptions) $ openConfigPage >> return ()
  let p = portNum userOptions
  bracket (openLocalState initialGameState)
          (createCheckpointAndClose)
          (startServer p)

startServer :: Int -> AcidState GameState -> IO ()
startServer p acid = do
  generateInitialMap acid
  simpleHTTP (conf {port = p}) (game acid)

conf = nullConf

game acid = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [ mzero
       , dir "start" $ startPages
       , dir "css" $ serveDirectory DisableBrowsing [] "html/css"
       , dir "js" $ serveDirectory DisableBrowsing [] "html/js"
       , dir "img" $ serveDirectory DisableBrowsing [] "html/img"
       , dir "a-img" $ serveDirectory DisableBrowsing [] "html/a-img"
       , notReady acid
       , nullDir >> homepage acid
       ]

notReady acid = do
  ready <- query' acid IsReady
  guard (not ready) -- if not ready, trigger this and block access to other pages
  ok . toResponse . header "Not Ready" $ do
    p ""

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

