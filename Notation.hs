module Notation where

import Types

data Diagram = Diagram
  { dBlack  :: String
  , dWhite  :: String
  , dPlayer :: Player
  , dMoves  :: [Move] -- optional, needed to handle deadlocks
  }

{-
Pieces are encoded this way:

1) Two first letters of color.
2) Coordinate.
3) Sumo status: ! (single), @ (double), # (triple), $ (quadruple)

BRh8 GRg8 REf8 YEe8 PId8 PUf5 BLb3 ORa8!

-}


parsePieces :: String -> [Piece]
