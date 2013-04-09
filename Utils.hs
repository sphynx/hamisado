module Utils where

import Data.List

groupIn :: Int -> [a] -> [[a]]
groupIn n = unfoldr f where
  f [] = Nothing
  f xs = Just $ splitAt n xs

-- Merge two sorted lists according to the predicate.
mergeSorted2 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeSorted2 _ xs [] = xs
mergeSorted2 _ [] ys = ys
mergeSorted2 predicate xx@(x:xs) yy@(y:ys) =
  if predicate x y
     then x : mergeSorted2 predicate xs yy
     else y : mergeSorted2 predicate xx ys

-- Merge list of sorted lists according to the predicate.
mergeSorted :: (a -> a -> Bool) -> [[a]] -> [a]
mergeSorted _ []  = []
mergeSorted _ [x] = x
mergeSorted predicate (x:xs) =
  mergeSorted2 predicate x (mergeSorted predicate xs)
