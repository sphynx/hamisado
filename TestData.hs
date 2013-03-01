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

