module Main where

import Analysis
import System.IO
import Text.Printf

main :: IO ()
main = do
       hSetBuffering stdout NoBuffering
       let depth = 9
       _ <- printf "depth = %d\n" depth
       print $ losingFirstMovesNS depth
