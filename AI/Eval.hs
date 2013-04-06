--
-- Static evaluation functions for game positions.
--

module AI.Eval
  ( simpleEval
  , threatBasedEval
  , evalFn
  , between
  ) where

import Game
import Types
import AI.Types

evalFn :: Evaluation -> (Round -> Int)
evalFn ThreatBasedEval = threatBasedEval
evalFn SimpleEval = simpleEval

simpleEval :: Round -> Int
simpleEval r = playerCoeff (rPlayer r) *
  case roundResult r of
    Winner Black -> posInfinity
    Winner White -> negInfinity
    InProgress   -> 0


-- This is based on the difference of the number of threating pieces
-- of each player ("threating" = can move to the last row in one
-- move). Not a terribly clever static evaluation, but it produces
-- sensible results and is easy enough to calculate.

-- Variation length is taken into into account in solved positions to
-- promote shortest wins and longest loses (yes, we are losing, but we
-- should fight until the end!)
threatBasedEval :: Round -> Int
threatBasedEval r = playerCoeff (rPlayer r) *

  case roundResult r of
    Winner Black -> posInfinity - variationLen
    Winner White -> negInfinity + variationLen
    InProgress   ->
      let b = rBoard r
          ba = attackersNumber Black b
          wa = attackersNumber White b
        in ba - wa

  where variationLen = length $ rMoves r

attackersNumber :: Board b => Player -> b -> Int
attackersNumber p b = length
   [ ()
   | from <- piecesCoords p b
   , not $ null $ threats p from b
   ]

playerCoeff :: Player -> Int
playerCoeff Black =  1
playerCoeff White = -1

threats :: Board b => Player -> Coord -> b -> [Coord]
threats player from board =
  [ target
  | target <- targets player from
  , freeWay from target board
  ]

targets :: Player -> Coord -> [Coord]
targets White (x,8) = [(x,8)]
targets White (x,y) =
      (if y >= 9 - x then [(x-(8-y), 8)] else [])
   ++ [(x,8)]
   ++ (if y >= x     then [(x+(8-y), 8)] else [])
targets Black (x,1) = [(x,1)]
targets Black (x,y) =
      (if y <= 9 - x then [(x+(y-1), 1)] else [])
   ++ [(x,1)]
   ++ (if y <= x     then [(x-(y-1), 1)] else [])

freeWay :: Board a => Coord -> Coord -> a -> Bool
freeWay from to b =
  all (flip fieldIsEmpty b) $ to : between from to

between :: Coord -> Coord -> [Coord]
between (x1,y1) (x2,y2)

  -- equal?
  | x1 == x2 && y1 == y2                             = []
  -- not on the same line?
  | abs(x1-x2) /= abs(y1-y2) && x1 /= x2 && y1 /= y2 = []

  -- vertical (2 cases)
  | x1 == x2 && y1 < y2  = [(x1, y) | y <- [y1+1 .. y2-1]]
  | x1 == x2 && y1 > y2  = symmetric

  -- horizontal (2 cases)
  | x1 < x2 && y1 == y2  = [(x, y1) | x <- [x1+1 .. x2-1]]
  | x1 > x2 && y1 == y2  = symmetric

  -- diagonal (4 cases)
  | x1 < x2 && y1 < y2   = [ (x,y) | x <- [x1+1 .. x2-1]
                           , let y = y1 + x - x1
                           ]
  | x1 < x2 && y1 > y2   = [ (x,y) | x <- [x1+1 .. x2-1]
                           , let y = y1 - x + x1
                           ]
  | x1 > x2              = symmetric
  | otherwise = error "Should not happen"

  where symmetric = between (x2,y2) (x1,y1)
