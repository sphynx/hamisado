module Types
 ( Color(..)
 , colors
 , Coord
 , coords
 , Move(..)
 , Depth
 , Score
 , Player(..)
 , Position(..)
 , Board(..)
 , Result(..)
 , opponent
 , posInfinity
 , negInfinity
 , parseMove
 ) where

import Data.Char
import Text.Printf

data Color
  = Brown
  | Green
  | Red
  | Yellow
  | Pink
  | Purple
  | Blue
  | Orange
  deriving (Eq, Ord, Show, Enum, Bounded)

colors :: [Color]
colors = [minBound .. maxBound]

type Coord = (Int, Int) -- 1 to 8

showFile :: Int -> String
showFile x = return $ chr $ 64 + x -- 1 to "A", 2 to "B" etc.

readXCoord :: Char -> Maybe Int
readXCoord c =
  let o = ord (toUpper c) - 64 -- 'A' to 1, 'B' to 2 etc.
  in if 1 <= o && o <= 8 then Just o else Nothing

readYCoord :: Char -> Maybe Int
readYCoord c =
  let o = ord c - 48 -- '1' to 1, etc.
  in if 1 <= o && o <= 8 then Just o else Nothing

coords :: [Coord]
coords = [(x, y) | x <- [1..8], y <- [1..8]]

data Player = Black | White
  deriving (Eq, Show, Enum, Bounded)

opponent :: Player -> Player
opponent White = Black
opponent Black = White

data Position b = Position
  { pBoard  :: b
  , pPlayer :: Player
  , pMoves  :: [Move]
  , pMoveNo :: Int
  } deriving (Eq, Show)

data Result = Winner Player | InProgress deriving (Eq, Show)

data Move = Move Coord Coord deriving (Eq, Ord, Read)

instance Show Move where
  show (Move (fx,fy) (tx,ty)) =
    printf "%s%d-%s%d" (showFile fx) fy (showFile tx) ty

-- Very crude parsing for the moment.
parseMove :: String -> Maybe Move
parseMove [x1, y1, '-', x2, y2] = do
  x1' <- readXCoord x1
  y1' <- readYCoord y1
  x2' <- readXCoord x2
  y2' <- readYCoord y2
  return $ Move (x1', y1') (x2', y2')
parseMove _ = Nothing

type Depth = Int
type Score = Int

-- We don't use minBound/maxBound here because it messes with algorithms in
-- Game_tree.
negInfinity :: Int
negInfinity = -1000000000

posInfinity :: Int
posInfinity = 1000000000

class Board a where
  board0       :: a
  updateBoard  :: Move -> a -> a
  fieldIsEmpty :: Coord -> a -> Bool
  fieldColor   :: Coord -> a -> Color
  pieceCoord   :: Player -> Color -> a -> Coord
  piecesCoords :: Player -> a -> [Coord]


