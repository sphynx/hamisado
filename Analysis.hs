{-# LANGUAGE RecordWildCards #-}

module Analysis
 ( solveWithPV
 , solveAB
 , losingFirstMovesNaive
 , losingFirstMovesAB
 , losingFirstMovesNS
 , losingFirstMovesPVs
 , SolvingResult(..)
 ) where

import Control.Arrow
import Game
import Types
import GameTreeSolver

data SolvingResult =
    Solved Player
  | Unknown
  deriving (Eq, Show)

solveAlphaBeta :: Depth -> Round -> SolvingResult
solveAlphaBeta d r = toSolvingResult $ snd $ solveAB r d

solveNegascout :: Depth -> Round -> SolvingResult
solveNegascout d r = toSolvingResult $ snd $ solveNS r d

solveWithPV :: Depth -> Round -> ([Round], SolvingResult)
solveWithPV d r = second toSolvingResult $ solveAB r d

toSolvingResult :: Int -> SolvingResult
toSolvingResult x = case x of
  100  -> Solved Black
  -100 -> Solved White
  _    -> Unknown

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

hasWon :: Player -> SolvingResult -> Bool
hasWon _   Unknown    = False
hasWon p1 (Solved p2) = p1 == p2


losingFirstMovesNaive :: Int -> [Move]
losingFirstMovesNaive depth =
  let variations = next start
      solutions = map (naiveSolve depth) variations
      varsols = variations `zip` solutions
  in [ head $ rMoves v | (v,s) <- varsols, Solved {} <- [s]]

losingFirstMovesAB :: Int -> [Move]
losingFirstMovesAB depth =
  let variations = next start
      solutions = map (solveAlphaBeta depth) variations
      varsols = variations `zip` solutions
  in [ head $ rMoves v | (v,s) <- varsols, Solved {} <- [s]]

losingFirstMovesNS :: Int -> [Move]
losingFirstMovesNS depth =
  let variations = next start
      solutions = map (solveNegascout depth) variations
      varsols = variations `zip` solutions
  in [ head $ rMoves v | (v,s) <- varsols, Solved {} <- [s]]

losingFirstMovesPVs :: Int -> [[Move]]
losingFirstMovesPVs depth =
     [ reverse $ rMoves (last pv)
     | r <- next start
     , let (pv, s) = solveWithPV depth r
     , Solved {} <- [s]
     ]
