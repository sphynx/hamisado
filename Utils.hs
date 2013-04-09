module Utils where

import Data.List

groupIn :: Int -> [a] -> [[a]]
groupIn n = unfoldr f where
  f [] = Nothing
  f xs = Just $ splitAt n xs

-- Merge two sorted lists according to the predicate.
merge2 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge2 _ xs [] = xs
merge2 _ [] ys = ys
merge2 predicate (x:xs) (y:ys) =
  if predicate x y
     then x : merge2 predicate xs (y:ys)
     else y : merge2 predicate (x:xs) ys

-- Merge list of sorted lists according to the predicate.
merge :: (a -> a -> Bool) -> [[a]] -> [a]
merge _ [] = []
merge _ (x:[]) = x
merge predicate (x:xs) = merge2 predicate x (merge predicate xs)
