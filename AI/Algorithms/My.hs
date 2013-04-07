{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}

module AI.Algorithms.My
 ( minimax
 , alphabeta
 , alphabeta2
 , negascout
 , GameTree(..)
 , Valuation
 ) where

import Types
import Control.Arrow
import Data.List
import Data.Ord

class GameTree a where
  is_terminal :: a -> Bool
  children    :: a -> [a]

type Valuation a = a -> Int

minimax :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
minimax valuation d t
  | d <= 0 || is_terminal t = (t, valuation t)
  | otherwise    = id *** negate $
                   minimumBy (comparing snd) $
                   [ minimax valuation (d-1) c | c <- children t ]

alphabeta :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
alphabeta val d t = alphabeta' val d t negInf posInf

alphabeta' :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
alphabeta' val d t _ _ | d == 0 || is_terminal t = (t, val t)
alphabeta' val d t alpha beta = go alpha t (children t) where

  go a pv [] = (pv, a)
  go a pv (c:cs) =
    let (cpv, score) = second negate $ alphabeta' val (d-1) c (-beta) (-a)
        res | score >= beta = (pv, beta)       -- beta-cutoff
            | score > a     = go score cpv cs  -- improved alpha
            | otherwise     = go a pv cs       -- nothing happened
    in res

alphabeta2 :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
alphabeta2 val d t = alphabeta2' val d t negInf posInf

alphabeta2' :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
alphabeta2' val d t _ _ | d == 0 || is_terminal t = (t, val t)
alphabeta2' val d t alpha beta = go alpha t (children t) where

  go a pv [] = (pv, a)
  go a pv (c:cs) =
    let (cpv, score) = second negate $ alphabeta2' val (d-1) c (-beta) (-a)
        res | score >= beta = (pv, beta)       -- beta-cutoff
            | score > a     = go score cpv cs  -- improved alpha
            | otherwise     = go a pv cs       -- nothing happened
    in res


negascout :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
negascout val d t = negascout' val d t negInf posInf

negascout' :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
negascout' val d t _ _ | d == 0 || is_terminal t = (t, val t)
negascout' val d t alpha beta = go alpha beta True t (children t) where

    go a _ _ pv [] = (pv, a)
    go !a !b isFirst pv (c:cs) =

     let (cpv, score) = second negate $ negascout' val (d-1) c (-b) (-a)
         (cpv', score')
           | a < score && score < beta && not isFirst =
                  -- re-search with 'beta' instead of 'b'
                  second negate $ negascout' val (d-1) c (-beta) (-a)
           | otherwise = (cpv, score)

         res | score' > a && score' >= beta = (cpv', score') -- beta cutoff
             | score' > a && score < beta = go score' (score' + 1) False cpv' cs
             | otherwise = go a (a + 1) False pv cs
     in res

posInf :: Int
posInf = maxBound

negInf :: Int
negInf = minBound + 1
