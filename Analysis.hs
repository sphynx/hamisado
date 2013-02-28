{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}


module Analysis
 ( solve
 , losingFirstMoves
 , losingSecondMoves
 , SolvingResult(..)
 ) where

import Game
import Types

type Depth = Int
type Solution = ()

data SolvingResult =
    Solved Player Solution
  | Unknown Depth -- analyzed up to that depth
  deriving (Eq, Show)

solve :: Int -> Round -> SolvingResult
solve _ Round{rResult = Winner p} = Solved p ()
solve 0 _ = Unknown 0
solve depth r =
  let i = rPlayer r
      he = alternate i
      iWon  = (i  `hasWon`)
      heWon = (he `hasWon`)
      solutions = map (solve (depth - 1)) $ next r

  in if any iWon solutions
     then Solved i ()
     else if all heWon solutions
          then Solved he ()
          else Unknown depth

hasWon :: Player -> SolvingResult -> Bool
hasWon _   Unknown{}    = False
hasWon p1 (Solved p2 _) = p1 == p2

losingFirstMoves :: Int -> [Move]
losingFirstMoves depth =
  let variations = next start
      solutions = map (solve depth) variations
      varsols = variations `zip` solutions
  in [ head $ rMoves v | (v,s) <- varsols, Solved {} <- [s]]


losingSecondMoves :: [[Move]]
losingSecondMoves =
  let depth = 3
      variations = forward 2 start
      solutions = map (solve depth) variations
      varsols = variations `zip` solutions
  in [ rMoves v | (v,s) <- varsols, Solved {} <- [s]]

{-

r1

to move: white
solved: white wins
solution:
  w g8-b3      -- only move (or many, but one is enough)
  b pass       -- any move
  w b3-d1 #

-}
