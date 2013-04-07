{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Game
  (
  -- Initial position.
    start

  -- Applying moves (one and many) to a position.
  , doMove
  , doMoves

  -- Legal moves in this position.
  , legalMoves

  -- Checks whether round is over and gets its final value.
  , isOver
  , roundResult

  -- Positions which may result from given position by applying
  -- certain number of legal moves.
  , legalPositions

  -- Positions which may result from given position by applying one
  -- move.
  , nextPositions
  ) where

import Data.List
import Types

start :: Round
start = Round
  { rBoard  = board0
  , rPlayer = Black
  , rMoves  = []
  , rMoveNo = 0
  }

doMove :: Move -> Round -> Round
doMove m Round {..} = Round
  { rBoard  = updateBoard m rBoard
  , rPlayer = opponent rPlayer
  , rMoves  = m : rMoves
  , rMoveNo = rMoveNo + 1
  }

doMoves :: [Move] -> Round -> Round
doMoves [] r = r
doMoves (m:ms) r =
  let r' = doMove m r
  in if isOver r then r' else doMoves ms r'

roundResult :: Round -> RoundResult
roundResult Round{..}
  | reachedHomeRow || isDeadlock = Winner $ opponent rPlayer
  | otherwise                    = InProgress

  where
    reachedHomeRow = case rMoves of
      [] -> False
      Move _ to :_ -> snd to == homeRow rPlayer

    isDeadlock = case rMoves of
      m:pm:_ -> isPass m && isPass pm
      _      -> False

isOver :: Round -> Bool
isOver r | Winner _ <- roundResult r = True
isOver _ = False

legalPositions :: Int -> Round -> [Round]
legalPositions 0 r = [r]
legalPositions d r =
  [ doMove m r | m <- sortMoves $ legalMoves r ] >>= legalPositions (d - 1)
  where
  sortMoves ms = applyLongFirstSort (rPlayer r) ms

nextPositions :: Round -> [Round]
nextPositions = legalPositions 1

-- Put longer moves first, since they are typically more forcing.
applyLongFirstSort :: Player -> [Move] -> [Move]
applyLongFirstSort p = sortBy f where
  f (Move _ (_,y1)) (Move _ (_,y2)) = case p of
    Black -> compare y2 y1
    White -> compare y1 y2


--- Move generation.
----------------------------------------

legalMoves :: Round -> [Move]
legalMoves r | isOver r = []
legalMoves r  = if null moves then passMove r else moves where
  moves = [ Move from to
          | from <- requiredFroms r
          , to <- possibleTos from (rPlayer r) (rBoard r)
          ]

possibleTos :: Board b => Coord -> Player -> b -> [Coord]
possibleTos (x,y) p b = case p of
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
requiredFrom (Move _ to) Round{..} =
  pieceCoord rPlayer (fieldColor to rBoard) rBoard

requiredFroms :: Round -> [Coord]
requiredFroms r = case rMoves r of
  []    -> initialFroms (rPlayer r)
  m : _ -> [ requiredFrom m r ]

initialFroms :: Player -> [Coord]
initialFroms Black = [(x,1) | x <- [1..8]]
initialFroms White = [(x,8) | x <- [1..8]]

passMove :: Round -> [Move]
passMove r = case rMoves r of
  []  -> []
  m:_ -> let from = requiredFrom m r
          in [Move from from]

isPass :: Move -> Bool
isPass (Move from to) = from == to

homeRow :: Player -> Int
homeRow Black = 1
homeRow White = 8

