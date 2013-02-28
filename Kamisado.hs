module Main where

import Data.Array
import Data.List
import Types
import Game
import Dia
import Analysis


import System.IO

--import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = do
       hSetBuffering stdout NoBuffering
       print $ losingFirstMoves 3

--main = defaultMain $ roundDiag $ head losingOpenings
--main = print losingSecondMoves
