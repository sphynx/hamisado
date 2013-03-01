{-# OPTIONS_GHC -fno-warn-orphans #-}

module GameTreeSolver where

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Types
import Game

instance Game_tree Round where
  is_terminal = isTerminal
  node_value r = gameValue (rPlayer r) r
  children = next

gameValue :: Player -> Round -> Int
gameValue Black r =
  case roundResult r of
    Winner Black -> 1
    Winner White -> -1
    InProgress   -> 0
gameValue White r = -gameValue Black r

solveAB :: Round -> Int -> ([Round], Int)
solveAB = alpha_beta_search

solveNS :: Round -> Int -> ([Round], Int)
solveNS = negascout
