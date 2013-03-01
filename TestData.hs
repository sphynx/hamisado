module TestData where

import Types
import Game

r1, r2, r3 :: Round
r1 = doMoves [Move (4,1) (4,7)] start
r2 = doMoves [Move (7,8) (2,3)] r1
r3 = doMoves [Move (4,7) (4,7)] r2

r4a, r4b, r4c, r4d :: Round
r4a = doMoves [Move (2,3) (1,2)] r3
r4b = doMoves [Move (2,3) (2,2)] r3
r4c = doMoves [Move (2,3) (3,2)] r3
r4d = doMoves [Move (2,3) (4,1)] r3


losingMoves3, losingMoves5 :: [Move]
losingMoves3 =
  [ Move (1,1) (1,7)
  , Move (2,1) (8,7)
  , Move (3,1) (1,3)
  , Move (4,1) (4,7)
  , Move (5,1) (5,7)
  , Move (7,1) (1,7)
  , Move (8,1) (8,7)
  ]

losingMoves5 =
  [ Move (1,1) (1,6)
  , Move (1,1) (1,7)
  , Move (2,1) (8,7)
  , Move (3,1) (1,3)
  , Move (4,1) (4,7)
  , Move (5,1) (5,7)
  , Move (7,1) (1,7)
  , Move (8,1) (8,6)
  , Move (8,1) (8,7)
  ]

{-

== Experiments and losing first moves found so far. ==

Params: negascout, d=7, moves sorted
Moves:  9: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3]
Time:   2.155s

Params: alphabeta, d=7, moves sorted
Moves:  9: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3]
Time:   2.52s

Params: negascout, d=9, moves sorted
Result: 9: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3]
Time:   17.36s

Params: alphabeta, d=9, moves sorted
Result: 9: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3]
Time:   19.83s

Params: negascout, d=11, moves sorted
Result: 10: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3,A1-B2]
Time:   2m8.022s

Params: alphabeta, d=11, moves sorted
Result: 10: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,C1-A3,A1-B2]
Time:   2m34.016s

Params: negascout, d=13, moves sorted
Moves:  23: [A1-A7,B1-H7,D1-D7,E1-E7,G1-A7,H1-H7,A1-A6,H1-H6,E1-A5,F1-B5,A1-D4,A1-C3,C1-A3,F1-D3,G1-E3,H1-F3,A1-B2,B1-A2,B1-C2,D1-C2,E1-F2,F1-G2,G1-F2]
Time:   17m28.199s

-}
