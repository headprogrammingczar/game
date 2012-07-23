{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module Lib.State where

#include "Imports.hs"

-- Either not ready (plus progress value)
-- Or the game state
data GameState = NoState Double | GameState {grid :: Grid, players :: Players, turnNumber :: Int} deriving (Eq, Ord, Read, Show, Data, Typeable)

data Player = Player {playerTurn :: PlayerTurn} deriving (Eq, Ord, Read, Show, Data, Typeable)

data Players = Players {player1, player2, player3, player4 :: Maybe Player} deriving (Eq, Ord, Read, Show, Data, Typeable)

data PlayerTurn = PlayerTurn deriving (Eq, Ord, Read, Show, Data, Typeable)

data Grid = Grid (Array (Int, Int) Square) deriving (Eq, Ord, Read, Show, Data, Typeable)

data Square = Black | Void | White | Tilde deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''GameState)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Players)
$(deriveSafeCopy 0 'base ''PlayerTurn)
$(deriveSafeCopy 0 'base ''Grid)
$(deriveSafeCopy 0 'base ''Square)

initialGameState :: GameState
initialGameState = NoState 0

readyState grid = GameState {grid = grid, players = noPlayers, turnNumber = 0}

noPlayers = Players Nothing Nothing Nothing Nothing

runTurn :: Update GameState ()
runTurn = do
  state <- get
  let newTurnNumber = 1 + turnNumber state
  put state {turnNumber = newTurnNumber}
  return ()

peekGrid :: Query GameState Grid
peekGrid = grid <$> ask

setGrid :: Grid -> Update GameState ()
setGrid gr = do
  state <- get
  put (state {grid = gr})

isReady :: Query GameState Bool
isReady = do
  st <- ask
  case st of
    NoState {} -> return False
    GameState {} -> return True

setReady :: Grid -> Update GameState ()
setReady grid = do
  put (readyState grid)

clearReady :: Update GameState ()
clearReady = do
  ready <- runQuery isReady
  when (not ready) $ do
    put (NoState 0)

incReady :: Double -> Update GameState Double
incReady inc = do
  (NoState d) <- get
  put (NoState (d + inc))
  return (d + inc)

$(makeAcidic ''GameState ['runTurn, 'peekGrid, 'setGrid, 'isReady, 'setReady, 'clearReady, 'incReady])

