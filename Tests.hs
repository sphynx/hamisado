{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Types
import TestData
import Game
import Analysis

import Data.Array.Unboxed
import Data.Bits
--import Data.Maybe
import Data.List

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Move generation tests" moveGenTests
  , testGroup "Solver tests" solverTests
  , testGroup "Long tests" longTests
  ]

moveGenTests =
  [ testCase "# of moves in starting position" test_moves_length1
  , testCase "# of moves in r1" test_moves_length2
  , testCase "# of moves in r2" test_moves_length3
  , testCase "# of positions after 2 plies" test_2_plies
  , testCase "# of positions after 3 plies" test_3_plies
  , testCase "# of positions after 4 plies" test_4_plies
  , testCase "# of positions after 5 plies" test_5_plies
  , testCase "Pass move" test_pass_move
  , testCase "Do pass move" test_do_pass_move
  , testCase "Is terminal?" is_terminal
  ]

test_moves_length1 = length (genMoves start) @?= 13*8-2  -- 102, checked manually
test_moves_length2 = length (genMoves r1) @?= 13
test_moves_length3 = length (genMoves r2) @?= 1
test_2_plies = length (forward 2 start) @?= 1188    -- not checked!
test_3_plies = length (forward 3 start) @?= 11969   -- not checked!
test_4_plies = length (forward 4 start) @?= 116828   -- not checked!
test_5_plies = length (forward 5 start) @?= 1044909  -- not checked!

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
  [ testCase "losing opening moves (naive, d=3)" test_losemoves_naive_d3
  , testCase "losing opening moves (alpha-beta, d=3)" test_losemoves_ab_d3
  , testCase "losing opening moves (alpha-beta, d=5)" test_losemoves_ab_d5
  , testCase "losing opening moves (negascout, d=3)" test_losemoves_ns_d3
  , testCase "losing opening moves (negascout, d=5)" test_losemoves_ns_d5
  ]

longTests =
  [ testCase "losing opening moves (naive, d=5)" test_losemoves_naive_d5
  , testCase "losing opening moves (alpha-beta, d=7)" test_losemoves_ab_d7
  , testCase "losing opening moves (alpha-beta, d=9)" test_losemoves_ab_d9
  , testCase "losing opening moves (negascout, d=7)" test_losemoves_ns_d7
  , testCase "losing opening moves (negascout, d=9)" test_losemoves_ns_d9

  ]


test_losemoves_naive_d3 = sort (losingFirstMovesNaive 3) @?= sort losingMoves3
test_losemoves_naive_d5 = sort (losingFirstMovesNaive 5) @?= sort losingMoves5

test_losemoves_ab_d3 = sort (losingFirstMovesAB 3) @?= sort losingMoves3
test_losemoves_ab_d5 = sort (losingFirstMovesAB 5) @?= sort losingMoves5
test_losemoves_ab_d7 = sort (losingFirstMovesAB 7) @?= sort losingMoves5
test_losemoves_ab_d9 = sort (losingFirstMovesAB 9) @?= sort losingMoves5

test_losemoves_ns_d3 = sort (losingFirstMovesNS 3) @?= sort losingMoves3
test_losemoves_ns_d5 = sort (losingFirstMovesNS 5) @?= sort losingMoves5
test_losemoves_ns_d7 = sort (losingFirstMovesNS 7) @?= sort losingMoves5
test_losemoves_ns_d9 = sort (losingFirstMovesNS 9) @?= sort losingMoves5


piecesCount :: Round -> Int
--piecesCount = length . catMaybes . map fPiece . elems . unNormalBoard . rBoard
piecesCount = sum . map fromIntegral . map ((.&.) 1) . elems . unBinaryBoard . rBoard


