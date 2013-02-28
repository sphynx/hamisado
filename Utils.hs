module Utils where

import Data.List

groupIn :: Int -> [a] -> [[a]]
groupIn n = unfoldr f where
  f [] = Nothing
  f xs = Just $ splitAt n xs
