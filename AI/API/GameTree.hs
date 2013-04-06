module AI.API.GameTree
  ( alphabeta
  , negascout
  , minimax
  ) where

import Types
import Game
import AI.Eval
import AI.Types

import Data.Tree.Game_tree.Game_tree
import qualified Data.Tree.Game_tree.Negascout as Algo

newtype GTSimple = GTSimple { unGTSimple :: Round }
instance Game_tree GTSimple where
  is_terminal = isOver . unGTSimple
  node_value  = simpleEval . unGTSimple
  children    = map GTSimple . nextPositions . unGTSimple

newtype GTThreatBased = GTThreatBased{ unGTThreatBased :: Round } deriving Show
instance Game_tree GTThreatBased where
  is_terminal = isOver . unGTThreatBased
  node_value  = threatBasedEval . unGTThreatBased
  children    = map GTThreatBased . nextPositions . unGTThreatBased


alphabeta :: Evaluation -> Round -> Depth -> (PV, Score)
alphabeta SimpleEval r d =
  let (pv, score) = Algo.alpha_beta_search (GTSimple r) d
      realPV = tail $ map (head . rMoves . unGTSimple) pv
  in (realPV, score)
alphabeta ThreatBasedEval r d =
  let (pv, score) = Algo.alpha_beta_search (GTThreatBased r) d
      realPV = tail $ map (head . rMoves . unGTThreatBased) pv
  in (realPV, score)


negascout :: Evaluation -> Round -> Depth -> (PV, Score)
negascout SimpleEval r d =
  let (pv, score) = Algo.negascout (GTSimple r) d
      realPV = tail $ map (head . rMoves . unGTSimple) pv
  in (realPV, score)
negascout ThreatBasedEval r d =
  let (pv, score) = Algo.negascout (GTThreatBased r) d
      realPV = tail $ map (head . rMoves . unGTThreatBased) pv
  in (realPV, score)



minimax :: Evaluation -> Round -> Depth -> (PV, Score)
minimax SimpleEval r d =
  let (pv, score) = Algo.negamax (GTSimple r) d
      realPV = tail $ map (head . rMoves . unGTSimple) pv
  in (realPV, score)
minimax ThreatBasedEval r d =
  let (pv, score) = Algo.negamax (GTThreatBased r) d
      realPV = tail $ map (head . rMoves . unGTThreatBased) pv
  in (realPV, score)

