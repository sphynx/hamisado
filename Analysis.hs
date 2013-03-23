{-# LANGUAGE RecordWildCards #-}

module Analysis
 ( losingFirstMoves
 , losingFirstMovesNaive
 , bestMove
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

naiveSolve :: Depth -> Round -> SolvingResult
naiveSolve _ r | Winner p <- roundResult r = Solved p
naiveSolve 0 _ = Unknown
naiveSolve depth r =
  let i = rPlayer r
      he = alternate i
      iWon  = (i  `hasWon`)
      heWon = (he `hasWon`)
      solutions = map (naiveSolve (depth - 1)) $ next r

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
  let variations = next start
      solutions = map (naiveSolve depth) variations
      varsols = variations `zip` solutions
  in [ head $ rMoves v | (v,s) <- varsols, Solved {} <- [s]]

losingFirstMoves :: Algorithm -> Implementation -> Depth -> [Move]
losingFirstMoves a i depth =
  let firstMoves = next start
      solutions = map (\r -> toSolvingResult $ snd $ search a i SimpleEval r depth) firstMoves
      movesSolutions = firstMoves `zip` solutions
  in [ head $ rMoves r | (r,s) <- movesSolutions, Solved {} <- [s]]

bestMove :: Algorithm -> Implementation -> Depth -> (PV, Int)
bestMove a i = search a i ThreatBasedEval start
