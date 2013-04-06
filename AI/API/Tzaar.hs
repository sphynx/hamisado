{-# OPTIONS_GHC -fno-warn-orphans #-}

module AI.API.Tzaar
  ( alphabeta
  , negascout
  , minimax
  ) where

import AI.Types
import AI.Eval
import Types
import Game

import qualified AI.Algorithms.Tzaar as Algo


alphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
alphabeta = runAlgo Algo.alphabeta

negascout :: Evaluation -> Round -> Depth -> (PV, Score)
negascout = runAlgo Algo.negascout

minimax :: Evaluation -> Round -> Depth -> (PV, Score)
minimax = runAlgo Algo.negamax


instance Algo.Gametree Round where
  is_terminal = isOver
  children = nextPositions

runAlgo :: ((Round -> Int) -> Depth -> Round -> Algo.Valued Round)
           -> Evaluation -> Round -> Depth -> (PV, Score)
runAlgo algoFn eval r d =
   let Algo.Valued score r1 = algoFn (evalFn eval) d r
       pv = drop (rMoveNo r) $ reverse (rMoves r1)
   in (pv, score)
