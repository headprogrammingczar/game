import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad
import Control.Monad.Cont
import System.Random
import Data.Array.IO

main = do
  unsafeInitGUIForThreadedRTS -- just for runhaskell
  mainWindow
  mainGUI

mainWindow = do
  window <- windowNew
  vbox <- vBoxNew False 3
  (img, canvas) <- newCanvas
  bar <- progressBarNew
  graph <- newGraph
  containerAdd window vbox
  boxPackStart vbox img PackNatural 0
  boxPackEnd vbox bar PackNatural 0
  onDestroy window mainQuit
  drawGraph canvas graph img
  widgetShowAll window
  runTest bar canvas graph img

newCanvas :: IO (Image, Pixmap)
newCanvas = do
  map <- pixmapNew (Nothing :: Maybe Drawable) 300 100 (Just 24)
  img <- imageNew
  set img [imagePixmap := map]
  return (img, map)

newGraph :: IO (IOUArray Int Int)
newGraph = newArray (0, 29) 0

yield = ContT $ \f -> (idleAdd (f () >> return False) priorityDefaultIdle) >> return ()

wait n = ContT $ \f -> (timeoutAdd (f () >> return False) n) >> return ()

-- we need to add the first part of the action as an idle callback, just in case
runCallback :: ContT () IO () -> IO ()
runCallback action = runContT (yield >> action) return

runTest bar canvas graph img = runCallback $ do
  forM_ [1..2000] $ \n -> do  
    liftIO $ do
      ix <- randomRIO (0, 29)
      modifyArray succ graph ix
      progressBarSetFraction bar (fromIntegral n / 2000)
      when (mod n 10 == 0) $ do
        drawGraph canvas graph img
    wait 25

drawGraph canvas graph img = do
  vs <- getAssocs graph
  renderWithDrawable canvas $ do
    setSourceRGB 1 1 1
    rectangle 0 0 300 100
    fill
    forM_ vs $ \(i, v) -> do
      let v' = fromIntegral v
          i' = fromIntegral i
      setSourceRGB 0 0 0
      rectangle (10 * i') (100 - v') 10 v'
      fill
  widgetQueueDraw img
  return ()

modifyArray f arr ix = do
  x <- readArray arr ix
  writeArray arr ix (f x)
  return ()

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

