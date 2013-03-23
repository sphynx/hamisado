{-# LANGUAGE FlexibleInstances #-}

module Algorithms where

import Types
import Control.Arrow
import Game hiding (isTerminal)
import qualified Game (isTerminal)
import Data.List
import Data.Ord

class GameTree a where
  isTerminal :: a -> Bool
  children   :: a -> [a]

type Valuation a = a -> Int

instance GameTree Round where
  isTerminal  = Game.isTerminal
  children    = next

nodeValue :: Round -> Int
nodeValue r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black ->  1
    Winner White -> -1
    InProgress   ->  0

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

finished :: GameTree a => Depth -> a -> Bool
finished d t = d <= 0 || isTerminal t

-- here we use the fact that max(a, b) = -min(-a, -b)
minimax :: GameTree a => Valuation a -> Depth -> a -> Int
minimax valuation d t
  | finished d t = valuation t
  | otherwise    = -minimum [minimax valuation (d-1) c | c <- children t]

minimaxPV :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
minimaxPV valuation d t
  | finished d t = (t, valuation t)
  | otherwise    = id *** negate $
                   minimumBy (comparing snd) $
                   [ minimaxPV valuation (d-1) c | c <- children t ]

posInf :: Int
posInf = maxBound

negInf :: Int
negInf = minBound + 1


ab :: GameTree a => Valuation a -> Depth -> a -> Int
ab valuation d t = snd $ alphabeta2 valuation d t

-- function negamax(node, depth, α, β, color)
--     if node is a terminal node or depth = 0
--         return color * the heuristic value of node
--     else
--         foreach child of node
--             val := -negamax(child, depth-1, -β, -α, -color)
--             {the following if statement constitutes alpha-beta pruning}
--             if val≥β
--                 return val
--             if val≥α
--                 α := val
--         return α


alphabeta_negamax :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
alphabeta_negamax val d t _ _ | d == 0 || isTerminal t = (t, val t)
alphabeta_negamax val d t alpha beta = go alpha t (children t) where

  go a pv [] = (pv, a)
  go a pv (c:cs) =
    let (cpv, score) = second negate $ alphabeta_negamax val (d-1) c (-beta) (-a)
        res | score >= beta = (pv, beta)
            | score > a     = go score cpv cs
            | otherwise     = go a pv cs
    in res


alphabeta_max :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
alphabeta_max val d t _ _ | d == 0 || isTerminal t = (t, val t)
alphabeta_max val d t alpha beta = go alpha t (children t) where

  go a pv [] = (pv, a)
  go a pv (c:cs) =
    let (cpv, score) = alphabeta_min val (d-1) c a beta
        res | score >= beta = (pv, beta)
            | score > a     = go score cpv cs
            | otherwise     = go a pv cs
    in res


alphabeta_min :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
alphabeta_min val d t _ _ | d == 0 || isTerminal t = (t, val t)
alphabeta_min val d t alpha beta = go beta t (children t) where

  go b pv [] = (pv, b)
  go b pv (c:cs) =
    let (cpv, score) = alphabeta_max val (d-1) c alpha b
        res | score <= alpha = (pv, alpha)
            | score <  b     = go score cpv cs
            | otherwise      = go b pv cs
    in res


alphabeta :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
alphabeta val d t = alphabeta_max val d t negInf posInf

alphabeta2 :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
alphabeta2 val d t = alphabeta_negamax val d t negInf posInf


data Tree a = Branch [Tree a]
            | Terminal a
              deriving (Show)

instance GameTree (Tree Int) where
  isTerminal (Terminal _) = True
  isTerminal _ = False

  children (Branch cs) = cs
  children (Terminal _) = []

directVal :: Tree Int -> Int
directVal (Branch _) = undefined
directVal (Terminal val) = val

b = Branch
t = Terminal

t1 :: Tree Int
t1 = b
       [ b [t 3, t 12, t 8]
       , b [t 2, t 4, t 6]
       , b [t 14, t 5, t 2]
       ]

t2 :: Tree Int
t2 = b
       [ b [t 9, t 8, t 7]
       , b [t 6, t 5, t 4]
       , b [t 3, t 3, t 1]
       ]

t3 :: Tree Int
t3 = b
       [ b [t 9, t 8, t 7]
       , b [t 6, undefined, undefined]
       , b [t 10, t 3, undefined, undefined]
       ]

test1 = minimax directVal 2 t1 == 3
test2 = minimax directVal 2 t2 == 7
test3 = ab directVal 2 t1 == 3
test4 = ab directVal 2 t3 == 7

tests = and [test1, test2, test3, test4]
