{-# LANGUAGE RecordWildCards #-}

module Game
  ( start       -- starting position
  , doMove      -- do move{,s}
  , doMoves
  , genMoves    -- generate list of possible moves
  , next        -- generate list of moves applied to positions (1 ply or more)
  , forward
  , roundResult -- is terminal?
  , isTerminal  -- is terminal?
  , threats
  , targets
  , between
  ) where

import Data.List
import Types

start :: Round
start = Round
  { rBoard  = board0
  , rPlayer = Black
  , rMoves  = []
  }

doMoves :: [Move] -> Round -> Round
doMoves (m:ms) r =
  let r' = doMove m r
  in case roundResult r' of
       InProgress -> doMoves ms r'
       _          -> r'
doMoves [] r = r

doMove :: Move -> Round -> Round
doMove m Round {..} = Round
  { rBoard  = updateBoard m rBoard
  , rMoves  = m : rMoves
  , rPlayer = alternate rPlayer
  }

roundResult :: Round -> RoundResult
roundResult Round{..}
  | reachedHomeRow || isDeadlock = Winner (alternate rPlayer)
  | otherwise                    = InProgress

  where
    reachedHomeRow = case rMoves of
      [] -> False
      Move _ to :_ -> snd to == homeRow rPlayer

    isDeadlock = case rMoves of
      m:pm:_ -> isPass m && isPass pm
      _      -> False

isTerminal :: Round -> Bool
isTerminal r =
  case roundResult r of
    Winner _ -> True
    _ -> False


forward :: Int -> Round -> [Round]
forward 0 r = [r]
forward d r =
  [ doMove m r | m <- sortMoves (rPlayer r) $ genMoves r ] >>= forward (d - 1)
  where
  sortMoves p =
    -- Put longer moves first, since they are typically more forcing.
    sortBy (\(Move _ (_,y1)) (Move _ (_,y2)) ->
               case p of
                 Black -> compare y2 y1
                 White -> compare y1 y2
             )

next :: Round -> [Round]
next = forward 1



--- Move generation.
----------------------------------------

genMoves :: Round -> [Move]
genMoves r | Winner _ <- roundResult r = []
genMoves r  = if null moves then passMove r else moves where
  moves = [ Move from to
          | from <- requiredFroms r
          , to <- genForwardMoves from (rPlayer r) (rBoard r)
          ]

genForwardMoves :: Board b => Coord -> Player -> b -> [Coord]
genForwardMoves (x,y) p b = case p of
  Black ->
       takeValid [ (x,  y+i) | i <- [1 .. 8-y] ]              -- straight up
    ++ takeValid [ (x-i,y+i) | i <- [1 .. min (x-1) (8-y)] ]  -- left up
    ++ takeValid [ (x+i,y+i) | i <- [1 .. min (8-x) (8-y)] ]  -- right up
  White ->
       takeValid [ (x,  y-i) | i <- [1 .. y-1] ]              -- straight down
    ++ takeValid [ (x-i,y-i) | i <- [1 .. min (x-1) (y-1)] ]  -- left down
    ++ takeValid [ (x+i,y-i) | i <- [1 .. min (8-x) (y-1)] ]  -- right down

  where
    takeValid = takeWhile (\to -> fieldIsEmpty to b)

requiredFrom :: Move -> Round -> Coord
requiredFrom lastMove Round{..} =
  pieceCoord rPlayer (colorOfToField rBoard lastMove) rBoard

requiredFroms :: Round -> [Coord]
requiredFroms r = case rMoves r of
  []  -> initialFroms (rPlayer r)
  m:_ -> [ requiredFrom m r ]

passMove :: Round -> [Move]
passMove r = case rMoves r of
  []  -> []
  m:_ -> let from = requiredFrom m r
          in [Move from from]

initialFroms :: Player -> [Coord]
initialFroms Black = [(x,1) | x <- [1..8]]
initialFroms White = [(x,8) | x <- [1..8]]

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

colorOfToField :: Board a => a -> Move -> Color
colorOfToField b (Move _ to) = fieldColor to b

isPass :: Move -> Bool
isPass (Move from to) = from == to

homeRow :: Player -> Int
homeRow Black = 1
homeRow White = 8

