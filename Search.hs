module Search
 ( alphaBeta1
 , alphaBeta2
 , negaScout1
 , negaScout2
 ) where

import Game
import Types

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

alphaBeta1 :: Round -> Depth -> ([Move], Score)
alphaBeta1 = alphaBeta GT1 unGT1

negaScout1 :: Round -> Depth -> ([Move], Score)
negaScout1 = negaScout GT1 unGT1

newtype GT1 = GT1 { unGT1 :: Round }
instance Game_tree GT1 where
  is_terminal = isTerminal . unGT1
  node_value  = absoluteValue . unGT1
  children    = map GT1 . next . unGT1

absoluteValue :: Round -> Int
absoluteValue r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity
    Winner White -> negInfinity
    InProgress   -> 0

alphaBeta2 :: Round -> Depth -> ([Move], Score)
alphaBeta2 = alphaBeta GT2 unGT2

negaScout2 :: Round -> Depth -> ([Move], Score)
negaScout2 = negaScout GT2 unGT2

newtype GT2 = GT2 { unGT2 :: Round }
instance Game_tree GT2 where
  is_terminal = isTerminal . unGT2
  node_value  = attackersUtility . unGT2
  children    = map GT2 . next . unGT2

attackersUtility :: Round -> Int
attackersUtility r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity
    Winner White -> negInfinity
    InProgress   ->
      let b = rBoard r
          ba = attackersNumber Black b
          wa = attackersNumber White b
        in ba - wa

attackersNumber :: Board b => Player -> b -> Int
attackersNumber p b = length
   [ from
   | from <- piecesCoords p b
   , not $ null $ threats p from b
   ]

-- Utils.

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

alphaBeta :: Game_tree a =>
             (Round -> a) -> (a -> Round)
             -> Round -> Depth -> ([Move], Score)
alphaBeta wrap unwrap r d =
  let (pv, score) = alpha_beta_search (wrap r) d
      moves = reverse $ rMoves $ unwrap $ last pv
  in (moves, score)

negaScout :: Game_tree a =>
             (Round -> a) -> (a -> Round)
             -> Round -> Depth -> ([Move], Score)
negaScout wrap unwrap r d =
  let (pv, score) = negascout (wrap r) d
      moves = reverse $ rMoves $ unwrap $ last pv
  in (moves, score)
