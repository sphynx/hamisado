module Board.Naive
 ( NaiveBoard(..)
 ) where

import Board.Common
import Types

import Data.Array
import Data.Maybe

newtype NaiveBoard =
  NaiveBoard { unNormalBoard :: Array Coord Field }
  deriving (Show)

instance Board NaiveBoard where

  board0 = NaiveBoard $ listArray ((1,1), (8,8)) initialPosition

  updateBoard (Move from to) b | from == to = b
  updateBoard (Move from to) (NaiveBoard b) =
    let Field fromColor fromPiece = b ! from
        Field toColor   _         = b ! to
    in NaiveBoard $ b //
            [ (to,   Field toColor   fromPiece)
            , (from, Field fromColor Nothing)
            ]

  fieldIsEmpty c (NaiveBoard b) = isNothing $ fPiece $ b ! c

  fieldColor c (NaiveBoard b) = fColor $ b ! c

  pieceCoord p color (NaiveBoard b) = head
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , piecePlayer piece == p
    , pieceColor piece == color
    ]

  piecesCoords p (NaiveBoard b) =
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , piecePlayer piece == p
    ]
