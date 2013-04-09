{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Analysis
 ( losingFirstMoves
 , losingFirstMovesNaive
 , bestMove
 , solve
 , SolvingResult(..)
 ) where

import AI
import Game
import Types

data SolvingResult =
    Solved Player
  | Unknown
  deriving (Eq, Show)

toSolvingResult :: Int -> SolvingResult
toSolvingResult x
  | x > 1000   = Solved Black
  | x < -1000  = Solved White
  | otherwise  = Unknown

solve :: Algorithm -> Implementation -> Depth ->  SolvingResult
solve a i d = toSolvingResult $ snd $
              search a i SimpleEval initialPosition d

naiveSolve :: Board b => Depth -> Position b -> SolvingResult
naiveSolve _ r | Winner p <- roundResult r = Solved p
naiveSolve 0 _ = Unknown
naiveSolve depth r =
  let i = pPlayer r
      he = opponent i
      iWon  = (i  `hasWon`)
      heWon = (he `hasWon`)
      solutions = map (naiveSolve (depth - 1)) $ nextPositions r

  in if any iWon solutions
     then Solved i
     else if all heWon solutions
          then Solved he
          else Unknown

  where
  hasWon :: Player -> SolvingResult -> Bool
  hasWon _   Unknown    = False
  hasWon p1 (Solved p2) = p1 == p2

losingFirstMovesNaive :: Int -> [Move]
losingFirstMovesNaive depth =
  let variations = nextPositions initialPosition
      solutions = map (naiveSolve depth) variations
      varsols = variations `zip` solutions
  in [ head $ pMoves v | (v,s) <- varsols, Solved {} <- [s]]

losingFirstMoves :: Algorithm -> Implementation -> Depth -> [Move]
losingFirstMoves a i depth =
  let firstMoves = nextPositions initialPosition
      solutions = map (\r -> toSolvingResult $ snd $ search a i SimpleEval r depth) firstMoves
      movesSolutions = firstMoves `zip` solutions
  in [ head $ pMoves r | (r,s) <- movesSolutions, Solved {} <- [s]]

bestMove :: Algorithm -> Implementation -> Depth -> (PV, Int)
bestMove a i = search a i ThreatBasedEval initialPosition
