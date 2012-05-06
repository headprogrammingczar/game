module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

newCanvas :: Int -> Int -> IO (Image, Pixmap)
newCanvas w h = do
  map <- pixmapNew (Nothing :: Maybe Drawable) w h (Just 24)
  img <- imageNew
  set img [imagePixmap := map]
  return (img, map)

scrollArea :: WidgetClass w => w -> IO ScrolledWindow
scrollArea inner = do
  area <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport area inner
  return area

yield = ContT $ \f -> (idleAdd (f () >> return False) priorityDefaultIdle) >> return ()

wait n = ContT $ \f -> (timeoutAdd (f () >> return False) n) >> return ()

initRun = ContT $ \f -> postGUIAsync (f ())

-- theoretically threadsafe
runCallback :: ContT () IO () -> IO ()
runCallback action = runContT (initRun >> action) return

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

