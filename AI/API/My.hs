{-# OPTIONS_GHC -fno-warn-orphans #-}

module AI.API.My
  ( alphabeta
  , negascout
  , minimax
  ) where

import Types
import Game
import AI.Eval
import AI.Types

import qualified AI.Algorithms.My as Algo

alphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
alphabeta = runAlgo Algo.alphabeta

negascout :: Evaluation -> Round -> Depth -> (PV, Score)
negascout = runAlgo Algo.negascout

minimax :: Evaluation -> Round -> Depth -> (PV, Score)
minimax = runAlgo Algo.minimax

instance Algo.GameTree Round where
  is_terminal = isOver
  children = nextPositions

runAlgo :: ((Round -> Int) -> Depth -> Round -> (Round, Score))
           -> Evaluation -> Round -> Depth -> (PV, Score)
runAlgo algoFn eval r d =
   let (r1, score) = algoFn (evalFn eval) d r
       pv = drop (rMoveNo r) $ reverse (rMoves r1)
   in (pv, score)

