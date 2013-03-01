{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Dia where

import Data.Array
import Data.Char
import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Game
import Types
import Utils

data FieldD a = FieldD
  (Colour a)         -- field color
  (Maybe (PieceD a)) -- piece description (if any)
  Bool               -- is from field?
  Bool               -- is to field?

data PieceD a = PieceD
  (Colour a)       -- piece color
  (Colour a)       -- player color
  SumoD            -- sumo description

data SumoD = SumoD -- TBD

f2f :: (Ord a, Floating a) => [Move] -> (Coord, Field) -> FieldD a
f2f lastMoves (fieldCoord, Field {..}) =
  let (isFrom, isTo) =
        case lastMoves of
         []               -> (False, False)
         Move from to : _ -> (from == fieldCoord, to == fieldCoord)
  in FieldD (c2c fColor) (fmap p2p fPiece) isFrom isTo

p2p :: (Ord a, Floating a) => Piece -> PieceD a
p2p Piece {..} = PieceD (c2c pColor) (c2c pPlayer) SumoD

roundDefs :: (Floating a, Ord a) => Round -> [[FieldD a]]
roundDefs Round{..}
  = map (reverse . map (f2f rMoves))
  . groupIn 8
  . assocs
  $ rBoard

roundDiag = hcat . map (vcat . map fieldDiag) . roundDefs

fieldDiag (FieldD color piece isFrom isTo) =
  maybe mempty pieceDiag piece
  <>
  roundedRect 1 1 0.1 # lw (if isSpecial then 0.06 else 0.02)
                      # fc color
                      # if isSpecial then lc red else id

  where isSpecial = isFrom || isTo

pieceDiag (PieceD piece player _) =
  circle 0.35 # lw 0.1
              # fc piece
              # lc player
c2c c
  = fromMaybe (error $ "Unrecognized color: " ++ show c)
  . readColourName
  . map toLower
  . show
  $ c
