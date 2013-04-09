{-# LANGUAGE RecordWildCards #-}

{- See the description of the binary format used for pieces/fields
encoding in Board.UArrayBased -}

module Board.VectorBased
  ( VBoard(..)
  ) where

import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Vector.Unboxed as V

import Board.Common
import Types

type W = Word16

newtype VBoard = VBoard (V.Vector W)

instance Board VBoard where

    board0 = VBoard $ V.fromList $ map field2bin initialPosition

    updateBoard b (Move from to) | from == to = b
    updateBoard (VBoard b) (Move from to) =
      let to_ix = vectorIdx to
          from_ix = vectorIdx from
          fromField = V.unsafeIndex b from_ix
          fromColor = bfcolor fromField
          fromPiece = fromField `shiftR` 4
          toColor   = bfcolor $ V.unsafeIndex b to_ix
      in VBoard $ V.unsafeUpd b
         [ (to_ix,   fromPiece `shiftL` 4 .|. toColor `shiftL` 1 .|. 1)
         , (from_ix, fromColor `shiftL` 1)
         ]

    fieldIsEmpty (VBoard b) c = bfempty $ V.unsafeIndex b $ vectorIdx c

    fieldColor (VBoard b) c = bin2color $ bfcolor $ V.unsafeIndex b $
                              vectorIdx c

    pieceCoord (VBoard b) player color =
      let bcolor = color2bin color
          bplayer = player2bin player
        in idxToCoord $ fromJust $
           V.findIndex (isPieceOfPlayerAndColor bcolor bplayer) b

    piecesCoords (VBoard b) player =
      let bplayer = player2bin player
        in map idxToCoord $ V.toList $ V.findIndices (isPieceOfPlayer bplayer) b

{-# INLINE isPieceOfPlayerAndColor #-}
isPieceOfPlayerAndColor :: W -> W -> W -> Bool
isPieceOfPlayerAndColor color player = \val ->
  let mask = (color `shiftL` 5) .|. (player `shiftL` 4) .|. 1
  in mask == val .&. 0xF1 -- 11110001

{-# INLINE isPieceOfPlayer #-}
isPieceOfPlayer :: W -> W -> Bool
isPieceOfPlayer player = \val ->
  let mask = (player `shiftL` 4) .|. 1
  in mask == val .&. 0x11 -- 00010001

vectorIdx :: (Int, Int) -> Int
vectorIdx (x,y) = (x-1) * 8 + (y-1)

idxToCoord :: Int -> (Int, Int)
idxToCoord i =
  let (q,r) = i `quotRem` 8
  in (q+1, r+1)

bfcolor :: W -> W
bfcolor w = w `shiftR` 1 .&. 7 {- 0b111 -}

bfempty :: W -> Bool
bfempty w = w .&. 1 == 0

field2bin :: Field -> W
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

bin2color :: W -> Color
bin2color = toEnum . fromIntegral

color2bin :: Color -> W
color2bin = fromIntegral . fromEnum

player2bin :: Player -> W
player2bin = fromIntegral . fromEnum
