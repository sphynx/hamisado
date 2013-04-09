{-# LANGUAGE RecordWildCards #-}

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

module Board.UArrayBased
  ( ABoard(..)
  ) where

import Data.Bits
import Data.Word
import Data.Array.Unboxed

import Board.Common
import Types

newtype ABoard = ABoard (UArray Coord Word8)
  deriving (Eq, Show)

instance Board ABoard where
  board0 = ABoard $
           listArray ((1,1), (8,8)) $
           map field2bin $ initialPosition

  {-# INLINE updateBoard #-}
  updateBoard b (Move from to) | from == to = b
  updateBoard (ABoard b) (Move from to) =
    let fromField = b ! from
        fromColor = bfcolor fromField
        fromPiece = fromField `shiftR` 4
        toColor   = bfcolor $ b ! to
    in ABoard $ b //
       [ (to,   fromPiece `shiftL` 4 .|. toColor `shiftL` 1 .|. 1)
       , (from, fromColor `shiftL` 1)
       ]

  {-# INLINE fieldIsEmpty #-}
  fieldIsEmpty (ABoard b) c = bfempty $ b ! c

  {-# INLINE fieldColor #-}
  fieldColor (ABoard b) c = bin2color $ bfcolor $ b ! c

  {-# INLINE pieceCoord #-}
  pieceCoord (ABoard b) p color = head
    [ coord
    | coord <- indices b
    , let val = b ! coord
    , let bcolor = color2bin color
    , let bplayer = player2bin p
    , let mask = (bcolor `shiftL` 5) .|. (bplayer `shiftL` 4) .|. 1
    , mask == val .&. 0xF1 -- 11110001
    ]

  {-# INLINE piecesCoords #-}
  piecesCoords (ABoard b) p =
    [ coord
    | coord <- indices b
    , let val = b ! coord
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

field2bin :: Field -> Word8
field2bin Field {..} = case fPiece of
  Nothing ->
    color2bin fColor `shiftL` 1
  Just (Piece {..}) ->
    (color2bin pieceColor `shiftL` 5)
    .|.
    (player2bin piecePlayer `shiftL` 4)
    .|.
    (color2bin fColor `shiftL` 1)
    .|.
    1

bin2color :: Word8 -> Color
bin2color = toEnum . fromIntegral

color2bin :: Color -> Word8
color2bin = fromIntegral . fromEnum

player2bin :: Player -> Word8
player2bin = fromIntegral . fromEnum
