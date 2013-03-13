{-# LANGUAGE FlexibleInstances #-}

module Algorithms where

import Types
import Control.Arrow
import Game hiding (isTerminal)
import qualified Game (isTerminal)
import Data.List
import Data.Ord
import Data.Tree

import Control.Monad.State.Lazy

class GameTree a where
  isTerminal :: a -> Bool
  utility    :: a -> Int
  children   :: a -> [a]

instance GameTree Round where
  isTerminal  = Game.isTerminal
  utility     = nodeValue
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

minimax :: GameTree a => Depth -> a -> Int
minimax d t
  | finished d t = utility t
  | otherwise    = -minimum [minimax (d-1) c | c <- children t]

minimaxPV :: GameTree a => Depth -> a -> ([a], Int)
minimaxPV d t
  | finished d t = return &&& utility $ t
  | otherwise    = (t:) *** negate $
                   minimumBy (comparing snd) $
                   [ minimaxPV (d-1) c | c <- children t ]

type ABState a = StateT (Int, Int) Maybe a

inf :: Int
inf = maxBound

negInf :: Int
negInf = minBound


ab :: GameTree a => Depth -> a -> Int
ab d t = alphabeta d t negInf inf


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


testPV' :: Int -> [[Move]]
testPV' depth =
     [ reverse $ rMoves (last pv)
     | c <- children start
     , let (pv, s) = minimaxPV depth c
     , abs s == 1
     ]



newtype TestTree = TestTree { unTree :: Tree Int }

instance GameTree TestTree where
  isTerminal = null . subForest . unTree
  utility    = rootLabel . unTree
  children   = map TestTree . subForest . unTree

instance Show TestTree where
  show = drawTree . fmap show . unTree


u = -100
n = Node
t v = Node v []

t1 :: TestTree
t1 = TestTree $ n u
       [ n u [t 3, t 12, t 8]
       , n u [t 2, t 4, t 6]
       , n u [t 14, t 5, t 2]
       ]


-- instance Show (Tree Int) where
--   show _ = ""
