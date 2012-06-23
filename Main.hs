{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}

#include "Lib/Imports.hs"
import Lib.State
import qualified System.Info
import System.Process (rawSystem)
import Control.Concurrent

main = do
  openConfigPage
  bracket (openLocalState initialGameState)
          (createCheckpointAndClose)
          (\acid -> simpleHTTP conf (game acid))

portNum = 12345
conf = nullConf {port = portNum}

game acid = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [ mzero
       , dir "start" $ startPages
       , nullDir >> homepage
       ]

homepage = ok . toResponse . H.html . H.body . p $ "boo!"

startPages = do
  msum [ mzero
       , dir "1" $ startOne
       , nullDir >> startOne
       ]

startOne = ok . toResponse . H.html . H.body . p $ "start page"

openConfigPage = forkIO $ do
  case System.Info.os of
    "mingw32" -> do
      rawSystem "start" ["http://localhost:"++ show portNum ++"/start/1"]
    "linux" -> do
      rawSystem "xdg-open" ["http://localhost:"++ show portNum ++"/start/1"]
  return ()

