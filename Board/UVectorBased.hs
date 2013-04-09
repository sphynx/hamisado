{-# LANGUAGE RecordWildCards #-}

module Board.UVectorBased
  ( VBoard(..)
  ) where

import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.Vector.Unboxed as V

import Board.Common
import Types

newtype VBoard = VBoard (V.Vector Word8)

instance Board VBoard where

    board0 = VBoard $ V.fromList $ map field2bin $ initialPosition

    updateBoard (Move from to) b | from == to = b
    updateBoard (Move from to) (VBoard b) =
      let to_ix = ix to
          from_ix = ix from
          fromField = V.unsafeIndex b from_ix
          fromColor = bfcolor fromField
          fromPiece = fromField `shiftR` 4
          toColor   = bfcolor $ V.unsafeIndex b to_ix
      in VBoard $ V.unsafeUpd b
         [ (to_ix,   fromPiece `shiftL` 4 .|. toColor `shiftL` 1 .|. 1)
         , (from_ix, fromColor `shiftL` 1)
         ]

    fieldIsEmpty c (VBoard b) = bfempty $ V.unsafeIndex b (ix c)

    fieldColor c (VBoard b) = bin2color $ bfcolor $ V.unsafeIndex b (ix c)

    pieceCoord player color (VBoard b) =
      let bcolor = color2bin color
          bplayer = player2bin player
        in cix $ fromJust $ V.findIndex (p bcolor bplayer) b

    piecesCoords player (VBoard b) =
      let bplayer = player2bin player
        in map cix $ V.toList $ V.findIndices (p2 bplayer) b


ix :: (Int, Int) -> Int
ix (x,y) = (x-1) * 8 + (y-1)

cix :: Int -> (Int, Int)
cix i = let (q,r) = i `quotRem` 8
        in (q+1, r+1)

{-# INLINE p #-}
p :: Word8 -> Word8 -> Word8 -> Bool
p color player = \val ->
  let mask = (color `shiftL` 5) .|. (player `shiftL` 4) .|. 1
  in mask == val .&. 0xF1 -- 11110001

{-# INLINE p2 #-}
p2 :: Word8 -> Word8 -> Bool
p2 player = \val ->
  let mask = (player `shiftL` 4) .|. 1
  in mask == val .&. 0x11 -- 00010001


bfcolor :: Word8 -> Word8
bfcolor w = w `shiftR` 1 .&. 7 {- 0b111 -}

bfempty :: Word8 -> Bool
bfempty w = w .&. 1 == 0

-- bpplayer :: Word8 -> Word8
-- bpplayer w = w `shiftR` 4 .&. 1

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
player2bin White = 0
player2bin Black = 1
