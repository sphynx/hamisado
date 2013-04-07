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

alphabeta :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
alphabeta = runAlgo Algo.alphabeta

negascout :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
negascout = runAlgo Algo.negascout

minimax :: Board b => Evaluation -> Position b -> Depth -> (PV, Score)
minimax = runAlgo Algo.minimax

instance Board b => Algo.GameTree (Position b) where
  is_terminal = isOver
  children = nextPositions

runAlgo :: Board b =>
           ((Position b -> Int) -> Depth -> Position b -> (Position b, Score))
           -> Evaluation -> Position b -> Depth -> (PV, Score)
runAlgo algoFn eval r d =
   let (r1, score) = algoFn (evalFn eval) d r
       pv = drop (pMoveNo r) $ reverse (pMoves r1)
   in (pv, score)

