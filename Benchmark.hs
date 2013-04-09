import Control.DeepSeq
import Criterion.Main

import AI
import Analysis
import Board.Naive
import Board.UVectorBased
import Board.UArrayBased
import Game
import Types

instance NFData Move where
  rnf (Move x y) = rnf x `seq` rnf y

losing :: Depth -> [Move]
losing = losingFirstMoves Negascout My

best :: Depth -> (PV, Int)
best = bestMove Negascout My

testMoves :: [Move]
testMoves =
  [ Move (6,1) (6,7)
  , Move (5,8) (5,3)
  , Move (3,1) (3,7)
  , Move (4,8) (6,6)
  , Move (1,1) (1,2)
  , Move (3,8) (8,3)
  , Move (2,1) (2,7)
  , Move (1,8) (1,3)
  , Move (7,1) (7,7)
  , Move (8,8) (8,4)
  , Move (5,1) (4,2)
  , Move (2,8) (1,7)
  , Move (3,7) (3,8)
  ]

{-
Number of entries of methods of Board class during profiling:

function     entries    inherited time, %
-----------------------------------------
board0       1          0.0%
updateBoard  2160288    5.7%
fieldIsEmpty 16426738   1.4%
fieldColor   2161130    2.5%
pieceCoord   2251130    34.7%
pieceCoords  ?          ?
-}

boardBench :: Board b => Position b -> Int
boardBench r0 =
  let -- 13 update boards
      b = pBoard $ doMoves testMoves r0
      -- 64 field is empty
      empties = length $ filter (fieldIsEmpty b) coords
      -- 16 field color
      reds = length [ () | x <- [1..2], y <- [1..8], let col = fieldColor b (x,y), col == Red]
      -- 16 piece coords
      x = sum [ x + y | p <- [Black, White], c <- colors, let (x,y) = pieceCoord b p c ]
   in empties + reds + x

main :: IO ()
main = defaultMain
 [ bgroup "losing"
    [ bench "d5" $ nf losing 5
    , bench "d7" $ nf losing 7
    , bench "d9" $ nf losing 9
    , bench "d11" $ nf losing 11
    ]

 , bgroup "best"
    [ bench "d5" $ nf best 5
    , bench "d7" $ nf best 7
    ]

 , bgroup "board"
    [ bcompare
    [ bench "normal"  $ nf boardBench (position0 :: Position NaiveBoard)
    , bench "uarray"  $ nf boardBench (position0 :: Position ABoard)
    , bench "uvector" $ nf boardBench (position0 :: Position VBoard)
    ]
    ]
 ]
