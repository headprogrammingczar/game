module Lib.Base where

import Lib.Imports
import System.IO.Unsafe (unsafeInterleaveIO)

-- everything
data GameState = Game {turnState :: TurnState, turnActions :: TurnActions, guiState :: GUIState}

-- important GUI stuff
data GUIState = GUIState {mainMapArea :: DrawingArea, viewportCenter :: (Int, Int)}

-- the state of the game at this turn
data TurnState = TurnState {mainMapGrid :: Grid}

-- what is going to happen between turns
data TurnActions = TurnActions

-- the game map is a 2D array of squares
data Square = Void | Filled
type Grid = IOArray (Int, Int) Square

type Game a = ContT () (ReaderT (IORef GameState) IO) a

askRef f = do
  ref <- ask
  gs <- liftIO $ readIORef ref
  return $ f gs

initialState :: IO GameState
initialState = do
  gui <- initialGUI
  turn <- initialTurn
  return $ Game turn emptyAction gui

initialGUI = do
  area <- drawingAreaNew
  return $ GUIState area (0, 0)

initialTurn :: IO TurnState
initialTurn = do
  map <- initialMap
  return $ TurnState map

-- the "do nothing" action
emptyAction = TurnActions

emptyMap :: IO Grid
emptyMap = newArray ((0, 0), (0, 0)) Void

initialMap :: IO Grid
initialMap = newArray ((0, 0), (5, 5)) Filled

-- the all-singing and dancing GTK run function
runGTK action = do
  unsafeInitGUIForThreadedRTS
  ref <- newIORef =<< initialState
  runReaderT (runContT action return) ref
  mainGUI

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

