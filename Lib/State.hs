{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module Lib.State where

#include "Imports.hs"

data GameState = GameState {count :: Int} deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''GameState)

initialGameState :: GameState
initialGameState = GameState 0

incCountBy :: Int -> Update GameState Int
incCountBy n = do
  c@GameState{..} <- get
  let newCount = count + n
  put $ c {count = newCount}
  return newCount

peekCount :: Query GameState Int
peekCount = count <$> ask

$(makeAcidic ''GameState ['incCountBy, 'peekCount])

handlers :: AcidState GameState -> ServerPart Response
handlers acid = msum
  [ dir "peek" $ do
      c <- query' acid PeekCount
      ok $ toResponse $ "peeked at the count and saw: " ++ show c
  , do
    nullDir
    c <- update' acid (IncCountBy 1)
    ok $ toResponse $ "New count is: " ++ show c
  ]

