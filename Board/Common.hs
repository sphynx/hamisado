module Board.Common
  ( Field(..)
  , Piece(..)
  , initialPosition
  ) where

import Data.List
import Types

data Field = Field
  { fColor :: Color
  , fPiece :: Maybe Piece
  } deriving (Show)

data Piece = Piece
  { piecePlayer :: Player
  , pieceColor  :: Color
  } deriving (Show)

initialPosition :: [Field]
initialPosition = concat $ transpose $
  [ [ filled minBound c | c <- colors ]
  , [ empty c | c <- [Purple, Brown, Yellow, Blue, Green, Pink, Orange, Red]]
  , [ empty c | c <- [Blue, Yellow, Brown, Purple, Red, Orange, Pink, Green]]
  , [ empty c | c <- [Yellow, Red, Green, Brown, Orange, Blue, Purple, Pink]]
  , [ empty c | c <- [Pink, Purple, Blue, Orange, Brown, Green, Red, Yellow]]
  , [ empty c | c <- [Green, Pink, Orange, Red, Purple, Brown, Yellow, Blue]]
  , [ empty c | c <- [Red, Orange, Pink, Green, Blue, Yellow, Brown, Purple]]
  , [ filled maxBound c | c <- reverse colors ]
  ] where
    empty :: Color -> Field
    empty = flip Field Nothing

    filled :: Player -> Color -> Field
    filled p c = Field c (Just $ Piece p c)
