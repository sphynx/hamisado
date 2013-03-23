{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Types
import TestData
import Game
import Analysis
import Algorithms
import Search

import Data.Array.Unboxed
import Data.Bits
import Data.List

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Move generation tests" moveGenTests
  , testGroup "Solver tests" solverTests
  , testGroup "Algorithms tests" algorithmTests
  , testGroup "My algorithms tests" myAlgorithmTests
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
  , testCase "between vertical 1" between_vertical_1
  , testCase "between vertical 2" between_vertical_2
  , testCase "between vertical 3" between_vertical_3
  , testCase "between vertical 4" between_vertical_4
  , testCase "between diagonal 1" between_diagonal_1
  , testCase "between diagonal 2" between_diagonal_2
  , testCase "between diagonal 3" between_diagonal_3
  , testCase "between diagonal 4" between_diagonal_4
  , testCase "between diagonal 5" between_diagonal_5
  , testCase "between symmetricity" between_symmetricity
  ]

test_moves_length1 = length (genMoves start) @?= 13*8-2  -- 102, checked manually
test_moves_length2 = length (genMoves r1) @?= 13
test_moves_length3 = length (genMoves r2) @?= 1
test_2_plies = length (forward 2 start) @?= 1150     -- not checked!
test_3_plies = length (forward 3 start) @?= 11182    -- not checked!
test_4_plies = length (forward 4 start) @?= 105024   -- not checked!
test_5_plies = length (forward 5 start) @?= 901006   -- not checked!

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

between_vertical_1 = between (1,1) (1,4) @?= [(1,2), (1,3)]
between_vertical_2 = between (2,4) (2,8) @?= [(2,5), (2,6), (2,7)]
between_vertical_3 = between (2,4) (2,5) @?= []
between_vertical_4 = between (2,4) (2,1) @?= [(2,2), (2,3)]

between_diagonal_1 = between (1,1) (3,3) @?= [(2,2)]
between_diagonal_2 = between (1,8) (3,6) @?= [(2,7)]
between_diagonal_3 = between (4,6) (1,3) @?= [(2,4), (3,5)]
between_diagonal_4 = between (2,7) (1,8) @?= []
between_diagonal_5 = between (6,2) (4,4) @?= [(5,3)]

between_symmetricity =
  assertBool "between x y == between y x" $
  and [ between from to == between to from
      | from <- coords, to <- coords
      ]

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
  , testCase "alphabeta verified on negamax (d=4, forward=1)" test_alpha_beta_d4_1
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


algorithmTests =
  [ testCase "alphabeta verified on negamax (d=2, forward=1)" test_alpha_beta_d2_1
  , testCase "alphabeta verified on negamax (d=3, forward=1)" test_alpha_beta_d3_1
  , testCase "alphabeta verified on negamax (d=2, forward=2)" test_alpha_beta_d2_2
  -- , testCase "negascout verified on alphabeta (d=2, forward=1)" test_negascout_d2_1
  -- , testCase "negascout verified on alphabeta (d=3, forward=1)" test_negascout_d3_1
  -- , testCase "negascout verified on alphabeta (d=4, forward=1)" test_negascout_d4_1
  -- , testCase "negascout verified on alphabeta (d=2, forward=2)" test_negascout_d2_2
  ]

test_alpha_beta_d2_1 = test_alpha_beta 2 1
test_alpha_beta_d3_1 = test_alpha_beta 3 1
test_alpha_beta_d4_1 = test_alpha_beta 4 1
test_alpha_beta_d2_2 = test_alpha_beta 2 2

test_negascout_d2_1 = test_negascout 2 1
test_negascout_d3_1 = test_negascout 3 1
test_negascout_d4_1 = test_negascout 4 1
test_negascout_d2_2 = test_negascout 2 2


test_alpha_beta d n =
  mapM_ (\r -> assertEqual "Alpha beta result does not correspond to negamax"
               ( negaMax2 r d) ( alphaBeta2 r d)) $
  forward n start

test_negascout d n =
  mapM_ (\r -> assertEqual "Negascout result does not correspond to alphabeta"
               (alphaBeta2 r d) (negaScout2 r d)) $
  forward n start


myAlgorithmTests =
  [ testCase "alphabeta verified on negamax (d=2, forward=1)" $ test_my_alpha_beta 2 1
  , testCase "alphabeta verified on negamax (d=3, forward=1)" $ test_my_alpha_beta 3 1
--  , testCase "alphabeta verified on negamax (d=4, forward=1)" $ test_my_alpha_beta 4 1
  , testCase "alphabeta verified on negamax (d=2, forward=2)" $ test_my_alpha_beta 2 2
--  , testCase "alphabeta verified on negamax (d=3, forward=2)" $ test_my_alpha_beta 3 2

  , testCase "negascout verified on alphabeta (d=2, forward=1)" $ test_my_negascout 2 1
  , testCase "negascout verified on alphabeta (d=3, forward=1)" $ test_my_negascout 3 1
  , testCase "negascout verified on alphabeta (d=4, forward=1)" $ test_my_negascout 4 1
  , testCase "negascout verified on alphabeta (d=2, forward=2)" $ test_my_negascout 2 2
  , testCase "negascout verified on alphabeta (d=3, forward=2)" $ test_my_negascout 3 2
  , testCase "negascout verified on alphabeta (d=4, forward=2)" $ test_my_negascout 4 2
  ]

test_my_alpha_beta d n =
  mapM_ (\r -> assertEqual "Alpha beta result does not correspond to negamax"
               (myMinimax attackersUtility d r) (myAlphaBeta attackersUtility d r)) $
  forward n start

test_my_negascout d n =
  mapM_ (\r -> assertEqual "Negascout result does not correspond to alphabeta"
               (myAlphaBeta attackersUtility d r)
               (myNegascout attackersUtility d r)) $
  forward n start
