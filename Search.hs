module Search
 ( alphaBeta1
 , alphaBeta2
 , negaScout1
 , negaScout2
 , negaMax2
 , attackersUtility
 ) where

import Game
import Types

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

alphaBeta1 :: Round -> Depth -> Score
alphaBeta1 r d = snd $ alpha_beta_search (GT1 r) d

negaScout1 :: Round -> Depth -> Score
negaScout1 r d = snd $ negascout (GT1 r) d

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
alphaBeta2 r d =
  let (pv, score) = alpha_beta_search (GT2 r) d
      realPV = tail $ map (head . rMoves . unGT2) pv
  in (realPV, score)

negaScout2 :: Round -> Depth -> ([Move], Score)
negaScout2 r d =
  let (pv, score) = negascout (GT2 r) d
      realPV = tail $ map (head . rMoves . unGT2) pv
  in (realPV, score)

negaMax2 :: Round -> Depth -> ([Move], Score)
negaMax2 r d =
  let (pv, score) = negamax (GT2 r) d
      realPV = tail $ map (head . rMoves . unGT2) pv
  in (realPV, score)


newtype GT2 = GT2 { unGT2 :: Round } deriving Show
instance Game_tree GT2 where
  is_terminal = isTerminal . unGT2
  node_value  = attackersUtility . unGT2
  children    = map GT2 . next . unGT2

attackersUtility :: Round -> Int
attackersUtility r = playerCoeff (rPlayer r) *
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
