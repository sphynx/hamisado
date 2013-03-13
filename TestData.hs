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
  , Move (6,1) (8,3)
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
  , Move (6,1) (8,3)
  , Move (7,1) (1,7)
  , Move (8,1) (8,6)
  , Move (8,1) (8,7)
  ]
