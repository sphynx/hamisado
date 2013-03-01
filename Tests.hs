{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Types
import TestData
import Game
import Analysis

import Data.Array
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Starting position tests" startingTests
  , testGroup "Basic solver tests" solverTests
  ]

startingTests =
  [ testCase "# of moves in starting position" test_moves_length1
  , testCase "# of moves in r1" test_moves_length2
  , testCase "# of moves in r2" test_moves_length3
  , testCase "# of positions after 2 plies" test_2_plies
  , testCase "Pass move" test_pass_move
  , testCase "Do pass move" test_do_pass_move
  , testCase "Is terminal?" is_terminal
  ]

test_moves_length1 = length (genMoves start) @?= 13*8-2
test_moves_length2 = length (genMoves r1) @?= 13
test_moves_length3 = length (genMoves r2) @?= 1
test_2_plies = length (forward 2 start) @?= 1188

test_pass_move = let [Move from to] = genMoves r2
                  in assertEqual "From == to in pass move" from to

test_do_pass_move =
  let [pass] = genMoves r2
      new = doMove pass r2
   in assertEqual
        "Pass move should not remove pieces from board"
        (piecesCount new)
        (piecesCount r2)

is_terminal = roundResult r4d @?= Winner White

solverTests =
  [ testCase "# of losing opening moves (d=3)" test_losing_moves_d3
  ]

test_losing_moves_d3 = length (losingFirstMoves 3) @?= 7

piecesCount :: Round -> Int
piecesCount = length . catMaybes . map fPiece . elems . rBoard


