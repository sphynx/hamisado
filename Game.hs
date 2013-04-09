{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Game
  (
  -- Initial position using default board representation.
    initialPosition

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

  -- Board representation.
  , module Board.UVectorBased
  ) where

import Types
import Utils

import Board.UVectorBased

position0 :: Board b => Position b
position0 = Position
  { pBoard  = board0
  , pPlayer = Black
  , pMoves  = []
  , pMoveNo = 0
  }

initialPosition :: Position VBoard
initialPosition = position0

doMove :: Board b => Move -> Position b -> Position b
doMove m Position {..} = Position
  { pBoard  = updateBoard m pBoard
  , pPlayer = opponent pPlayer
  , pMoves  = m : pMoves
  , pMoveNo = pMoveNo + 1
  }

doMoves :: Board b => [Move] -> Position b -> Position b
doMoves [] r = r
doMoves (m:ms) r =
  let r' = doMove m r
  in if isOver r then r' else doMoves ms r'

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
isOver r | Winner _ <- roundResult r = True
isOver _ = False

{-# SPECIALIZE legalPositions :: Int -> Position VBoard -> [Position VBoard] #-}
legalPositions :: Board b => Int -> Position b -> [Position b]
legalPositions 0 r = [r]
legalPositions d r = [ doMove m r | m <- legalMoves r ] >>= legalPositions (d - 1)

nextPositions :: Board b => Position b -> [Position b]
nextPositions = legalPositions 1

--- Move generation.
----------------------------------------

legalMoves :: Board b => Position b -> [Move]
legalMoves r | isOver r = []
legalMoves r  = if null moves then passMove r else moves where
  moves = [ Move from to
          | from <- requiredFroms r
          , to <- possibleTos from (pPlayer r) (pBoard r)
          ]

possibleTos :: Board b => Coord -> Player -> b -> [Coord]
possibleTos (x,y) p b = case p of
  -- Generate move in nicely sorted order, so that we don't need to
  -- sort them later. We put longer moves first, since they are
  -- typically more forcing.
  Black -> reverse $ merge (\ (_,y1) (_,y2) -> y1 < y2)
      [ takeValid [ (x,  y+i) | i <- [1 .. 8-y] ]              -- straight up
      , takeValid [ (x-i,y+i) | i <- [1 .. min (x-1) (8-y)] ]  -- left up
      , takeValid [ (x+i,y+i) | i <- [1 .. min (8-x) (8-y)] ]  -- right up
      ]
  White -> reverse $ merge (\ (_,y1) (_,y2) -> y1 > y2)
      [ takeValid [ (x,  y-i) | i <- [1 .. y-1] ]              -- straight down
      , takeValid [ (x-i,y-i) | i <- [1 .. min (x-1) (y-1)] ]  -- left down
      , takeValid [ (x+i,y-i) | i <- [1 .. min (8-x) (y-1)] ]  -- right down
      ]

  where
    takeValid = takeWhile (\to -> fieldIsEmpty to b)

requiredFrom :: Board b => Move -> Position b -> Coord
requiredFrom (Move _ to) Position{..} =
  pieceCoord pPlayer (fieldColor to pBoard) pBoard

requiredFroms :: Board b => Position b -> [Coord]
requiredFroms r = case pMoves r of
  []    -> initialFroms (pPlayer r)
  m : _ -> [ requiredFrom m r ]

initialFroms :: Player -> [Coord]
initialFroms Black = [(x,1) | x <- [1..8]]
initialFroms White = [(x,8) | x <- [1..8]]

passMove :: Board b => Position b -> [Move]
passMove r = case pMoves r of
  []  -> []
  m:_ -> let from = requiredFrom m r
          in [Move from from]

isPass :: Move -> Bool
isPass (Move from to) = from == to

homeRow :: Player -> Int
homeRow Black = 1
homeRow White = 8

