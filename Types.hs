module Types where

import Data.Array
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

coords :: [Coord]
coords = [(x, y) | x <- [1..8], y <- [1..8]]

data Player = Black | White
  deriving (Eq, Show, Enum, Bounded)

alternate :: Player -> Player
alternate White = Black
alternate Black = White

data Piece = Piece
  { pPlayer     :: Player
  , pColor      :: Color
  , pSumoStatus :: Sumo
  } deriving (Show)

data Sumo = None | Single | Double | Triple | Quadruple
  deriving (Eq, Show, Enum)

data Field = Field
  { fColor :: Color
  , fPiece :: Maybe Piece
  } deriving (Show)

type Board = Array Coord Field

data Round = Round
  { rBoard  :: Board
  , rPlayer :: Player
  , rMoves  :: [Move]
  } deriving Show

data RoundResult = Winner Player | InProgress deriving (Eq, Show)

data Game = Game
  { gPlayMethod :: PlayMethod
  , gRounds     :: [Round]
  , gResult     :: GameResult
  } deriving Show

data PlayMethod = SingleRound | Standard | Long | Marathon deriving (Eq, Show)

data GameResult = Score Int Int deriving (Eq, Show)

data Move = Move Coord Coord deriving (Eq, Ord)

instance Show Move where
  show (Move (fx,fy) (tx,ty)) =
    printf "%s%d-%s%d" (showFile fx) fy (showFile tx) ty

data Direction = LTR | RTL deriving (Eq, Show)

type Depth = Int
