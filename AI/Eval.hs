--
-- Static evaluation functions for positions.
--

module AI.Eval (absoluteVal, threatBasedVal) where

import Game
import Types

absoluteVal :: Round -> Int
absoluteVal r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity
    Winner White -> negInfinity
    InProgress   -> 0

threatBasedVal :: Round -> Int
threatBasedVal r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity - variationLen
    Winner White -> negInfinity + variationLen
    InProgress   ->
      let b = rBoard r
          ba = attackersNumber Black b
          wa = attackersNumber White b
        in ba - wa

  where variationLen = length $ rMoves r

attackersNumber :: Board b => Player -> b -> Int
attackersNumber p b = length
   [ from
   | from <- piecesCoords p b
   , not $ null $ threats p from b
   ]

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

