module Board.Naive
 ( NaiveBoard(..)
 ) where

import Board.Common
import Types

import Data.Array
import Data.Maybe

newtype NaiveBoard = NaiveBoard (Array Coord Field)
  deriving (Show)

instance Board NaiveBoard where

  board0 = NaiveBoard $ listArray ((1,1), (8,8)) initialPosition

  updateBoard b (Move from to) | from == to = b
  updateBoard (NaiveBoard b) (Move from to) =
    let Field fromColor fromPiece = b ! from
        Field toColor   _         = b ! to
    in NaiveBoard $ b //
            [ (to,   Field toColor   fromPiece)
            , (from, Field fromColor Nothing)
            ]

  fieldIsEmpty (NaiveBoard b) c = isNothing $ fPiece $ b ! c

  fieldColor (NaiveBoard b) c = fColor $ b ! c

  pieceCoord (NaiveBoard b) p color = head
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , piecePlayer piece == p
    , pieceColor piece == color
    ]

  piecesCoords (NaiveBoard b) p =
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , piecePlayer piece == p
    ]
