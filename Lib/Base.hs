module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

data GameState = Game TurnState TurnActions
data TurnState = TurnState Int Int
data TurnActions = TurnActions

type Game a = ContT () (ReaderT (IORef GameState) IO) a

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
  runReaderT (runContT action return) ref
  mainGUI

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

-- give gtk a moment to do its own stuff, then continue
yield :: Game ()
yield = ContT $ \k -> ReaderT $ \ref -> idleAdd (runReaderT (k ()) ref >> return False) priorityDefaultIdle >> return ()

delay :: Int -> Game ()
delay n = ContT $ \k -> ReaderT $ \ref -> timeoutAdd (runReaderT (k ()) ref >> return False) n >> return ()

runCallback :: Game () -> Game (IO ())
runCallback action = do
  r <- ask
  return $ runReaderT (runContT action return) r

