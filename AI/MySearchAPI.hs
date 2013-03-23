module AI.MySearchAPI
  ( alphabeta
  , negascout
  ) where

import AITypes
import Types
import Game
import AI.Eval

import qualified AI.Algorithms.My as Algo

alphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
alphabeta ThreatBasedEval r d =
  Algo.alphabeta threatBasedVal r d

negascout :: Evaluation -> Round -> Depth -> (PV, Score)
negascout = undefined

instance Algo.GameTree Round where
  is_terminal = isTerminal
  children = next
