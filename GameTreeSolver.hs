{-# OPTIONS_GHC -fno-warn-orphans #-}

module GameTreeSolver where

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Types
import Game

instance Game_tree Round where
  is_terminal = isTerminal
  node_value  = nodeValue
  children    = next

nodeValue :: Round -> Int
nodeValue r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black ->  100
    Winner White -> -100
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

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

solveAB :: Round -> Int -> ([Round], Int)
solveAB = alpha_beta_search

solveNS :: Round -> Int -> ([Round], Int)
solveNS = negascout
