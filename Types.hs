{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Array
import Data.Char
import Data.Word
import Data.List
import Data.Maybe
import Text.Printf
import Data.Bits
import qualified Data.Array.Unboxed as U

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

readXCoord :: Char-> Int
readXCoord c = ord (toUpper c) - 64 -- 'A' to 1, 'B' to 2 etc.

readYCoord :: Char-> Int
readYCoord c = ord c - 48 -- '1' to 1, etc.


coords :: [Coord]
coords = [(x, y) | x <- [1..8], y <- [1..8]]

data Player = Black | White
  deriving (Eq, Show, Enum, Bounded)

alternate :: Player -> Player
alternate White = Black
alternate Black = White

data Round = Round
  { rBoard  :: BinaryBoard
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

data Move = Move Coord Coord deriving (Eq, Ord, Read)

instance Show Move where
  show (Move (fx,fy) (tx,ty)) =
    printf "%s%d-%s%d" (showFile fx) fy (showFile tx) ty

-- Very crude parsing for the moment.
parseMove :: String -> Move
parseMove [x1, y1, '-', x2, y2] =
  Move (readXCoord x1, readYCoord y1) (readXCoord x2, readYCoord y2)
parseMove x = error $ "Cannot parse " ++ x

data Direction = LTR | RTL deriving (Eq, Show)

type Depth = Int
type Score = Int

-- We don't use minBound/maxBound here because it messes with algorithms in
-- Game_tree.
negInfinity :: Int
negInfinity = -1000000000

posInfinity :: Int
posInfinity = 1000000000

class Board a where
  board0 :: a
  updateBoard :: Move -> a -> a
  fieldIsEmpty :: Coord -> a -> Bool
  fieldColor :: Coord -> a -> Color
  pieceCoord :: Player -> Color -> a -> Coord
  piecesCoords :: Player -> a -> [Coord]

{-# SPECIALIZE board0 :: BinaryBoard #-}
{-# SPECIALIZE updateBoard :: Move -> BinaryBoard -> BinaryBoard #-}
{-# SPECIALIZE fieldIsEmpty :: Coord -> BinaryBoard -> Bool #-}
{-# SPECIALIZE fieldColor :: Coord -> BinaryBoard -> Color #-}
{-# SPECIALIZE pieceCoord :: Player -> Color -> BinaryBoard -> Coord #-}
{-# SPECIALIZE piecesCoords :: Player -> Color -> BinaryBoard -> Coord #-}

--- Normal board

newtype NormalBoard =
  NormalBoard { unNormalBoard :: Array Coord Field }
  deriving (Show)

data Field = Field
  { fColor :: Color
  , fPiece :: Maybe Piece
  } deriving (Show)

data Piece = Piece
  { pPlayer :: Player
  , pColor  :: Color
  } deriving (Show)

instance Board NormalBoard where

  board0 = NormalBoard $ listArray ((1,1), (8,8)) initialPosition

  updateBoard (Move from to) b | from == to = b
  updateBoard (Move from to) (NormalBoard b) =
    let Field fromColor fromPiece = b ! from
        Field toColor   _         = b ! to
    in NormalBoard $ b //
            [ (to,   Field toColor   fromPiece)
            , (from, Field fromColor Nothing)
            ]

  fieldIsEmpty c (NormalBoard b) = isNothing $ fPiece $ b ! c

  fieldColor c (NormalBoard b) = fColor $ b ! c

  pieceCoord p color (NormalBoard b) = head
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , pPlayer piece == p
    , pColor piece == color
    ]

  piecesCoords p (NormalBoard b) =
    [ coord
    | coord <- indices b
    , Just piece <- [fPiece $ b ! coord]
    , pPlayer piece == p
    ]


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


--- Binary board

newtype BinaryBoard =
  BinaryBoard { unBinaryBoard :: U.UArray Coord Word8 }
  deriving (Show)

{-

In BinaryBoard each field is represented as a byte (Word8). All 8 bits
are in use (we will need to move to Word16 when we need sumo rings
support).

This allows us to use unboxed arrays and make some checks on bit
level, which gives ~15% speed increase. We should probably switch to
mutable arrays to get more impressive speedup.

Bit format description:

- Last (8th, rightmost) bit: empty (0) or filled with a piece (1)
- 5th - 7th: three bits for field color (8 possible colors)
- 4th bit: player (white or black) if filled, 0 if not filled
- 1st - 3rd: three bits for piece color, 000 if not filled

Example:
00000101

000-0-010-1
Last 1 means that this field is filled
Field color is "010", which is Red.
0 means White.
First "000" means Brown.

To summarise: this byte encodes red field with brown piece of White
player on it.

-}

instance Board BinaryBoard where
  board0 = BinaryBoard $
           U.listArray ((1,1), (8,8)) $
           map field2bin $ initialPosition

  updateBoard (Move from to) b | from == to = b
  updateBoard (Move from to) (BinaryBoard b) =
    let fromField = b U.! from
        fromColor = bfcolor fromField
        fromPiece = fromField `shiftR` 4
        toColor   = bfcolor $ b U.! to
    in BinaryBoard $ b U.//
       [ (to,   fromPiece `shiftL` 4 .|. toColor `shiftL` 1 .|. 1)
       , (from, fromColor `shiftL` 1)
       ]

  fieldIsEmpty c (BinaryBoard b) = bfempty $ b U.! c

  fieldColor c (BinaryBoard b) = bin2color $ bfcolor $ b U.! c

  pieceCoord p color (BinaryBoard b) = head
    [ coord
    | coord <- U.indices b
    , let val = b U.! coord
    , let bcolor = color2bin color
    , let bplayer = player2bin p
    , not $ bfempty val
    , bplayer == bpplayer val
    , bcolor == bpcolor val
    ]

  piecesCoords p (BinaryBoard b) =
    [ coord
    | coord <- U.indices b
    , let val = b U.! coord
    , let bplayer = player2bin p
    , not $ bfempty val
    , bplayer == bpplayer val
    ]


bfcolor :: Word8 -> Word8
bfcolor w = w `shiftR` 1 .&. 7 {- 0b111 -}

bfempty :: Word8 -> Bool
bfempty w = w .&. 1 == 0

bpplayer :: Word8 -> Word8
bpplayer w = w `shiftR` 4 .&. 1

bpcolor :: Word8 -> Word8
bpcolor w = w `shiftR` 5

field2bin :: Field -> Word8
field2bin Field {..} = case fPiece of
  Nothing ->
    color2bin fColor `shiftL` 1
  Just (Piece {..}) ->
    (color2bin pColor `shiftL` 5)
    .|.
    (player2bin pPlayer `shiftL` 4)
    .|.
    (color2bin fColor `shiftL` 1)
    .|.
    1

bin2field :: Word8 -> Field
bin2field w =
  let empty  = w .&. 1 == 0
      fcolor = bin2color  $ w `shiftR` 1 .&. 7
      player = bin2player $ w `shiftR` 4 .&. 1
      pcolor = bin2color  $ w `shiftR` 5
  in if empty then Field fcolor Nothing
              else Field fcolor (Just $ Piece player pcolor)

bin2color :: Word8 -> Color
bin2color = toEnum . fromIntegral

color2bin :: Color -> Word8
color2bin = fromIntegral . fromEnum

player2bin :: Player -> Word8
player2bin White = 0
player2bin Black = 1

bin2player :: Word8 -> Player
bin2player 0 = White
bin2player 1 = Black
bin2player x = error $ "Unexpected binary for player" ++ show x

