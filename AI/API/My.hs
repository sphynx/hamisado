{-# OPTIONS_GHC -fno-warn-orphans #-}

module AI.API.My
  ( alphabeta
  , idAlphabeta
  , negascout
  , minimax
  ) where

import Types
import Game
import AI.Eval
import AI.Types
import Data.List

import qualified AI.Algorithms.My as Algo

alphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
alphabeta = runAlgo Algo.alphabeta

negascout :: Evaluation -> Round -> Depth -> (PV, Score)
negascout = runAlgo Algo.negascout

minimax :: Evaluation -> Round -> Depth -> (PV, Score)
minimax = runAlgo Algo.minimax


instance Algo.GameTree Round where
  is_terminal = isTerminal
  children t h = nextWithHint h t


runAlgo :: ((Round -> Int) -> Depth -> Round -> (Round, Score))
           -> Evaluation -> Round -> Depth -> (PV, Score)
runAlgo algoFn eval r d =
   let (r1, score) = algoFn (evalFn eval) d r
       pv = drop (rMoveNo r) $ reverse (rMoves r1)
   in (pv, score)


idAlphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
idAlphabeta e r d
  | d < 3 = alphabeta e r d
  | otherwise =
      let s0 = Algo.alphabeta (evalFn e) 1 r
          (r1, score) = foldl' (\acc depth -> Algo.alphabeta2 (evalFn e) depth (Just $ fst acc) r) s0 [2..d]
          pv = drop (rMoveNo r) $ reverse (rMoves r1)
      in (pv, score)

