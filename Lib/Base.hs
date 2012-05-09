module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

-- everything
data GameState = Game TurnState TurnActions

-- the state of the game at this turn
data TurnState = TurnState Grid

-- what is going to happen between turns
data TurnActions = TurnActions

-- the game map is a 2D array of squares
data Square = Void
type Grid = IOArray (Int, Int) Square

type Game a = ContT () (ReaderT (IORef GameState) IO) a

initialState :: IO GameState
initialState = do
  turn <- initialTurn
  return $ Game turn emptyAction

initialTurn :: IO TurnState
initialTurn = do
  map <- emptyMap
  return $ TurnState map

-- the "do nothing" action
emptyAction = TurnActions

emptyMap = newArray ((0, 0), (0, 0)) Void

-- the all-singing and dancing GTK run function
runGTK action = do
  unsafeInitGUIForThreadedRTS
  ref <- newIORef =<< initialState
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

