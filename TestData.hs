module TestData where

import Types
import Game

r1, r2, r3 :: Round
r1 = doMoves [Move (D,1) (D,7)] start
r2 = doMoves [Move (G,8) (B,3)] r1
r3 = doMoves [Move (D,7) (D,7)] r2

r4a, r4b, r4c, r4d :: Round
r4a = doMoves [Move (B,3) (A,2)] r3
r4b = doMoves [Move (B,3) (B,2)] r3
r4c = doMoves [Move (B,3) (C,2)] r3
r4d = doMoves [Move (B,3) (D,1)] r3

