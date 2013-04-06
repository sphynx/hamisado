{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Analysis
import AI
import AI.Eval
import Game
import TestData
import Types

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Text.Printf

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Move generation tests" moveGenTests
  , testGroup "Losing opening moves tests" losingMovesTests

  -- algorithms
  , testGroup "GameTree algorithms tests" gameTreeAlgorithmTests
  , testGroup "My algorithms tests" myAlgorithmTests
  , testGroup "Tzaar algorithms tests" tzaarAlgorithmTests

  -- these take too long, so they are usually not run too often
  , testGroup "Long running tests" longTests
  ]


------------------------------------------------------------
-- Move generator tests.

moveGenTests =
  [ perft 1 102
  , perft 2 1150
  , perft 3 11182
  , perft 4 105024
  , perft 5 901006
  , testCase "# of moves in r1" test_moves_length2
  , testCase "# of moves in r2" test_moves_length3
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

test_moves_length2 = length (legalMoves r1) @?= 13
test_moves_length3 = length (legalMoves r2) @?= 1

perft depth leaves =
  testCase description $ length (legalPositions depth start) @?= leaves where
    description = printf "perft d=%d (# of game tree leaves)" depth

test_pass_move = let [Move from to] = legalMoves r2
                  in assertEqual "From == to in pass move" from to

test_do_pass_move =
  let [pass] = legalMoves r2
      new = doMove pass r2
   in assertEqual
        "Pass move should not remove pieces from board"
        (piecesCount new)
        (piecesCount r2)

piecesCount :: Round -> Int
piecesCount = sum . map fromIntegral . map ((.&.) 1) . elems . unBinaryBoard . rBoard


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

------------------------------------------------------------
-- "Losing moves" tests. I.e. tests which calculate which starting
-- moves are losing and compares them with the baseline.

losingMovesTests =
  [ testCase "losing opening moves (naive, d=3)" test_losemoves_naive_d3
  , test_losing_moves AlphaBeta GameTree 3
  , test_losing_moves AlphaBeta My 3
  , test_losing_moves AlphaBeta Tzaar 3
  , test_losing_moves AlphaBeta GameTree 5
  , test_losing_moves AlphaBeta My 5
  , test_losing_moves Negascout GameTree 3
  , test_losing_moves Negascout My 3
  , test_losing_moves Negascout Tzaar 3
  , test_losing_moves Negascout GameTree 5
  , test_losing_moves Negascout Tzaar 5
  , test_losing_moves Negascout My 5
  , test_losing_moves Negascout My 7
  ]


test_losing_moves a i d =
  testCase description $
  sort (losingFirstMoves a i d) @?= sort baseline

  where
    description = printf "losing opening moves (%s, %s, d=%d)"
                  (show a) (show i) d

    baseline | d < 3 = []
             | d == 3 = losingMoves3
             | d > 3 && d <= 9 = losingMoves5
             | d == 11 = losingMoves11
             | d == 13 = losingMoves13
             | otherwise = error $ printf "No baseline losing moves for depth %d" d

test_losemoves_naive_d3 = sort (losingFirstMovesNaive 3) @?= sort losingMoves3
test_losemoves_naive_d5 = sort (losingFirstMovesNaive 5) @?= sort losingMoves5


------------------------------------------------------------
-- Long running tests.

longTests =
  [ testCase "losing opening moves (naive, d=5)" test_losemoves_naive_d5
  , test_losing_moves AlphaBeta GameTree 7
  , test_losing_moves AlphaBeta GameTree 9
  , test_losing_moves Negascout GameTree 7
  , test_losing_moves Negascout GameTree 8
  , test_losing_moves Negascout My 11
  , test_losing_moves AlphaBeta My 11
  , test_losing_moves Negascout GameTree 11
  , test_losing_moves AlphaBeta GameTree 11
  , test_losing_moves Negascout My 13
  , test_alpha_beta GameTree 4 1
  , test_negascout My 3 2
  , test_negascout My 4 2
  , test_alpha_beta Tzaar 4 1
  , test_negascout_score Tzaar 3 2
  , test_negascout_score Tzaar 4 2
  , perft 6 7399924
  , perft 7 56183354
  -- , perft 8 409591124 -- too long even for long tests
  ]


------------------------------------------------------------
-- Tests of AI algorithms (AlphaBeta, Negascout, Minimax) from
-- game-tree Hackage package.

gameTreeAlgorithmTests =
  [ test_alpha_beta GameTree 2 1
  , test_alpha_beta GameTree 3 1
  , test_alpha_beta GameTree 2 2

  , test_negascout_score GameTree 2 1
  , test_negascout_score GameTree 3 1
  , test_negascout_score GameTree 4 1
  , test_negascout_score GameTree 2 2
  ]


------------------------------------------------------------
-- Tests of my implementation of AI algorithms (AlphaBeta, Negascout,
-- Minimax).

myAlgorithmTests =
  [ test_alpha_beta My 2 1
  , test_alpha_beta My 3 1
  , test_alpha_beta My 2 2
  , test_negascout My 2 1
  , test_negascout My 3 1
  , test_negascout My 4 1
  , test_negascout My 2 2
  ]

------------------------------------------------------------
-- Tests of AI algorithms (AlphaBeta, Negascout, Minimax) from
-- hstzaar.

tzaarAlgorithmTests =
  [ test_alpha_beta Tzaar 2 1
  , test_alpha_beta Tzaar 3 1
  , test_alpha_beta Tzaar 2 2
  , test_negascout_score Tzaar 2 1
  , test_negascout_score Tzaar 3 1
  , test_negascout_score Tzaar 4 1
  , test_negascout_score Tzaar 2 2
  ]

------------------------------------------------------------
-- Common function for testing algorithms.

test_alpha_beta i d n =
  testCase description $
  mapM_ (\r -> assertEqual
               "Alpha beta result does not correspond to negamax"
               (search Minimax i ThreatBasedEval r d)
               (search AlphaBeta i ThreatBasedEval r d)) $
  legalPositions n start

  where description =
          printf "alphabeta verified on negamax (impl=%s, d=%d, forward=%d)"
          (show i) d n

test_negascout i d n =
  testCase description $
  mapM_ (\r -> assertEqual
               "Negascout result does not correspond to alphabeta"
               (search AlphaBeta i ThreatBasedEval r d)
               (search Negascout i ThreatBasedEval r d)) $
  legalPositions n start

  where description =
          printf "negascout verified on alphabeta (impl=%s, d=%d, forward=%d)"
          (show i) d n

-- Here we test only score, since PV may differ and it fact they do
-- (in GameTree due to a bug - most likely, in hstzaar - due to
-- algorithm peculiarities)
test_negascout_score i d n =
  testCase description $
  mapM_ (\r -> assertEqual
               "Negascout score does not correspond to alphabeta"
               (snd $ search AlphaBeta i ThreatBasedEval r d)
               (snd $ search Negascout i ThreatBasedEval r d)) $
  legalPositions n start

  where description =
          printf "negascout score verified on alphabeta (impl=%s, d=%d, forward=%d)"
          (show i) d n

