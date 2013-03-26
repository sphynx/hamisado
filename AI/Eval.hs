--
-- Static evaluation functions for game positions.
--

module AI.Eval
  ( simpleEval
  , threatBasedEval
  , evalFn
  ) where

import Game
import Types
import AI.Types

evalFn :: Evaluation -> (Round -> Int)
evalFn ThreatBasedEval = threatBasedEval
evalFn SimpleEval = simpleEval

simpleEval :: Round -> Int
simpleEval r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity
    Winner White -> negInfinity
    InProgress   -> 0


-- This is based on the difference of the number of threating pieces
-- of each player ("threating" = can move to the last row in one
-- move). Not a terribly clever static evaluation, but it produces
-- sensible results and is easy enough to calculate.

-- Variation length is taken into into account in solved positions to
-- promote shortest wins and longest loses (yes, we are losing, but we
-- should fight until the end!)
threatBasedEval :: Round -> Int
threatBasedEval r = playerCoeff (rPlayer r) *

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
   [ ()
   | from <- piecesCoords p b
   , not $ null $ threats p from b
   ]

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

