{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Game
  (
  -- Initial position using default board representation and its type.
    initialPosition
  , GamePosition

  -- Initial position (polymorphic).
  , position0

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

import Types
import Utils

import Board.VectorBased

type GamePosition = Position VBoard

position0 :: Board b => Position b
position0 = Position
  { pBoard  = board0
  , pPlayer = Black
  , pMoves  = []
  , pMoveNo = 0
  }

initialPosition :: GamePosition
initialPosition = position0

doMove :: Board b => Move -> Position b -> Position b
doMove m Position {..} = Position
  { pBoard  = updateBoard pBoard m
  , pPlayer = opponent pPlayer
  , pMoves  = m : pMoves
  , pMoveNo = pMoveNo + 1
  }

doMoves :: Board b => [Move] -> Position b -> Position b
doMoves [] p = p
doMoves (m:ms) p =
  let p' = doMove m p
  in if isOver p then p' else doMoves ms p'

{-# SPECIALIZE roundResult :: Position VBoard -> Result #-}
roundResult :: Board b => Position b -> Result
roundResult Position{..}
  | reachedHomeRow || isDeadlock = Winner $ opponent pPlayer
  | otherwise                    = InProgress

  where
    reachedHomeRow = case pMoves of
      [] -> False
      Move _ to :_ -> snd to == homeRow pPlayer

    isDeadlock = case pMoves of
      m:pm:_ -> isPass m && isPass pm
      _      -> False

isOver :: Board b => Position b -> Bool
isOver p | Winner _ <- roundResult p = True
isOver _ = False

{-# SPECIALIZE legalPositions :: Int -> Position VBoard -> [Position VBoard] #-}
legalPositions :: Board b => Int -> Position b -> [Position b]
legalPositions 0 p = [p]
legalPositions d p = [ doMove m p | m <- legalMoves p ] >>= legalPositions (d - 1)

nextPositions :: Board b => Position b -> [Position b]
nextPositions = legalPositions 1

--- Move generation.
----------------------------------------

legalMoves :: Board b => Position b -> [Move]
legalMoves r | isOver r = []
legalMoves r = if null moves then passMove r else moves where
  moves = [ Move from to
          | from <- requiredFroms r
          , to <- possibleTos from (pPlayer r) (pBoard r)
          ]

possibleTos :: Board b => Coord -> Player -> b -> [Coord]
possibleTos (x,y) p b =
  -- Generate moves in the nicely sorted order, so that we don't need
  -- to sort them later. We put longer moves first, since they are
  -- typically more forcing.
  reverse $ mergeSorted (longerFirst p snd) $
  case p of
    Black ->
      [ takeValid [ (x,  y+i) | i <- [1 .. 8-y] ]              -- straight up
      , takeValid [ (x-i,y+i) | i <- [1 .. min (x-1) (8-y)] ]  -- left up
      , takeValid [ (x+i,y+i) | i <- [1 .. min (8-x) (8-y)] ]  -- right up
      ]
    White ->
      [ takeValid [ (x,  y-i) | i <- [1 .. y-1] ]              -- straight down
      , takeValid [ (x-i,y-i) | i <- [1 .. min (x-1) (y-1)] ]  -- left down
      , takeValid [ (x+i,y-i) | i <- [1 .. min (8-x) (y-1)] ]  -- right down
      ]

  where
    takeValid = takeWhile (fieldIsEmpty b)

longerFirst :: Player -> (a -> Int) -> a -> a -> Bool
longerFirst p f y1 y2 = case p of
  Black -> f y1 < f y2
  White -> f y1 > f y2

requiredFrom :: Board b => Position b -> Move -> Coord
requiredFrom Position{..} (Move _ to) =
  pieceCoord pBoard pPlayer (fieldColor pBoard to)

requiredFroms :: Board b => Position b -> [Coord]
requiredFroms p = case pMoves p of
  []    -> initialFroms (pPlayer p)
  m : _ -> [ requiredFrom p m ]

initialFroms :: Player -> [Coord]
initialFroms Black = [(x,1) | x <- [1..8]]
initialFroms White = [(x,8) | x <- [1..8]]

passMove :: Board b => Position b -> [Move]
passMove p = case pMoves p of
  []  -> []
  m:_ -> let from = requiredFrom p m
          in [Move from from]

isPass :: Move -> Bool
isPass (Move from to) = from == to

homeRow :: Player -> Int
homeRow Black = 1
homeRow White = 8

