module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

newCanvas :: IO (Image, Pixmap)
newCanvas = do
  map <- pixmapNew (Nothing :: Maybe Drawable) 300 100 (Just 24)
  img <- imageNew
  set img [imagePixmap := map]
  return (img, map)

yield = ContT $ \f -> (idleAdd (f () >> return False) priorityDefaultIdle) >> return ()

wait n = ContT $ \f -> (timeoutAdd (f () >> return False) n) >> return ()

runCallback :: ContT () IO () -> IO ()
runCallback action = runContT (yield >> action) return

safeSpark :: IO a -> IO (IO a)
safeSpark act = do
  var <- newEmptyMVar
  forkIO $ do
    val <- act
    putMVar var val
  return (takeMVar var)

spark :: IO a -> IO a
spark act = do
  var <- newEmptyMVar
  forkIO $ do
    val <- act
    putMVar var val
  unsafeInterleaveIO (takeMVar var)

