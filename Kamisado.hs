module Main where

import Analysis
import System.IO

main :: IO ()
main = do
       hSetBuffering stdout NoBuffering
       print $ losingFirstMoves 5
