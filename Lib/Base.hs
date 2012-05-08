module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

data GameState = Game TurnState TurnActions
data TurnState = TurnState Int Int
data TurnActions = TurnActions

type Game a = ReaderT (IORef GameState) (ContT () IO) a

initialState :: IO (IORef GameState)
initialState = do
  ref <- newIORef emptyState
  return ref

emptyState = Game emptyTurn emptyAction

emptyTurn = TurnState 0 0
emptyAction = TurnActions

-- the all-singing and dancing GTK run function
runGTK action = do
  unsafeInitGUIForThreadedRTS
  ref <- initialState
  runContT (runReaderT action ref) return
  mainGUI

newCanvas :: Int -> Int -> IO (Image, Pixmap)
newCanvas w h = do
  map <- pixmapNew (Nothing :: Maybe Drawable) w h (Just 24)
  img <- imageNew
  set img [imagePixmap := map]
  return (img, map)

scrollArea :: WidgetClass w => w -> Int -> Int -> IO ScrolledWindow
scrollArea inner w h = do
  area <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport area inner
  widgetSetSizeRequest area w h
  return area

-- give gtk a moment to do its own stuff, then continue
yield :: Game ()
yield = do
  callCC $ \k -> do
    next <- runCallback $ k ()
    liftIO $ idleAdd (next >> return False) priorityDefaultIdle
    return ()

runCallback :: Game () -> Game (IO ())
runCallback action = do
  r <- ask
  return (runContT (runReaderT action r) return)

