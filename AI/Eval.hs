--
-- Static evaluation functions for positions.
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
   [ from
   | from <- piecesCoords p b
   , not $ null $ threats p from b
   ]

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

