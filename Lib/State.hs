{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module Lib.State where

#include "Imports.hs"

data GameState = GameState {grid :: Grid, players :: Players, turnNumber :: Int} deriving (Eq, Ord, Read, Show, Data, Typeable)

data Player = Player {playerTurn :: PlayerTurn} deriving (Eq, Ord, Read, Show, Data, Typeable)

data Players = Players {player1, player2, player3, player4 :: Maybe Player} deriving (Eq, Ord, Read, Show, Data, Typeable)

data PlayerTurn = PlayerTurn deriving (Eq, Ord, Read, Show, Data, Typeable)

data Grid = Grid (Array (Int, Int) Square) deriving (Eq, Ord, Read, Show, Data, Typeable)

data Square = Void | White deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''GameState)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Players)
$(deriveSafeCopy 0 'base ''PlayerTurn)
$(deriveSafeCopy 0 'base ''Grid)
$(deriveSafeCopy 0 'base ''Square)

initialGameState :: GameState
initialGameState = GameState {grid = voidGrid, players = noPlayers, turnNumber = 0}

voidGrid = Grid $ listArray ((0, 0), (299, 199)) (repeat Void)

noPlayers = Players Nothing Nothing Nothing Nothing

runTurn :: Update GameState ()
runTurn = do
  state <- get
  let newTurnNumber = 1 + turnNumber state
  put state {turnNumber = newTurnNumber}
  return ()

peekGrid :: Query GameState Grid
peekGrid = grid <$> ask

$(makeAcidic ''GameState ['runTurn, 'peekGrid])

