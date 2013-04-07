{-# OPTIONS_GHC -fno-warn-orphans #-}

module AI.API.Tzaar
  ( alphabeta
  , negascout
  , minimax
  ) where

import AI.Eval
import AI.Types
import Game
import Types

import qualified AI.Algorithms.Tzaar as Algo

alphabeta :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
alphabeta = runAlgo Algo.alphabeta

negascout :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
negascout = runAlgo Algo.negascout

minimax :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
minimax = runAlgo Algo.negamax

instance Board b => Algo.Gametree (Position b) where
  is_terminal = isOver
  children = nextPositions

runAlgo :: Board b =>
           ((Position b -> Int) -> Depth -> Position b -> Algo.Valued (Position b))
           -> Evaluation -> Position b -> Depth -> (PV, Score)
runAlgo algoFn eval r d =
   let Algo.Valued score r1 = algoFn (evalFn eval) d r
       pv = drop (pMoveNo r) $ reverse (pMoves r1)
   in (pv, score)
