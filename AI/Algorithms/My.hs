{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}

module AI.Algorithms.My
 ( minimax
 , alphabeta
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


negascout :: GameTree a => Valuation a -> Depth -> a -> (a, Int)
negascout val d t = negascout' val d t negInf posInf

negascout' :: GameTree a => Valuation a -> Depth -> a -> Int -> Int -> (a, Int)
negascout' val d r _ _ | d == 0 || is_terminal r = (r, val r)
negascout' val d r alpha beta = go alpha beta True r (children r) where

    go a _ _ pv [] = (pv, a)
    go a b isFirst pv (c:cs) =

     let (cpv, score) = second negate $ negascout' val (d-1) c (-b) (-a)
         (cpv', score') | a < score && score < beta && not isFirst =
                     -- research with 'beta' instead of 'b'
                     second negate $ negascout' val (d-1) c (-beta) (-a)
                | otherwise = (cpv, score)

         -- new_a = max a score'
         -- res | new_a >= beta = new_a  -- beta cutoff
         --     | otherwise = go new_a (new_a + 1) False cs

         -- res | max a score' >= beta = max a score'
         --     | score' > a = go score' (score' + 1) False cs
         --     | otherwise = go a (a + 1) False cs

         res | score' > a && score' >= beta = (cpv', score')
             | score' > a && score < beta = go score' (score' + 1) False cpv' cs
             | a >= score' && a >= beta = (pv, a)
             | otherwise = go a (a + 1) False pv cs
     in res

posInf :: Int
posInf = maxBound

negInf :: Int
negInf = minBound + 1


--
-- Basic testing.
--
data Tree a = Branch [Tree a]
            | Terminal a
              deriving (Show)

instance GameTree (Tree Int) where
  is_terminal (Terminal _) = True
  is_terminal _ = False

  children (Branch cs) = cs
  children (Terminal _) = []

directVal :: Tree Int -> Int
directVal (Branch _) = undefined
directVal (Terminal val) = val

br = Branch
term = Terminal

t1 :: Tree Int
t1 = br
       [ br [term 3, term 12, term 8]
       , br [term 2, term 4, term 6]
       , br [term 14, term 5, term 2]
       ]

t2 :: Tree Int
t2 = br
       [ br [term 9, term 8, term 7]
       , br [term 6, term 5, term 4]
       , br [term 3, term 3, term 1]
       ]

t3 :: Tree Int
t3 = br
       [ br [term 9, term 8, term 7]
       , br [term 6, undefined, undefined]
       , br [term 10, term 3, undefined, undefined]
       ]

test1 = snd (minimax directVal 2 t1) == 3
test2 = snd (minimax directVal 2 t2) == 7
test3 = snd (alphabeta directVal 2 t1) == 3
test4 = snd (alphabeta directVal 2 t3) == 7
test5 = snd (negascout directVal 2 t1) == 3
test6 = snd (negascout directVal 2 t3) == 7

tests = and [test1, test2, test3, test4, test5, test6]
