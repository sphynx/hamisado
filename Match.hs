{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Match
 ( match
 , humanStrategy
 , aiStrategy
 , analysisStrategy
 ) where

import Types
import Game
import AI
import Text.Printf

type PlayStrategy = Position VBoard -> IO Move

match :: PlayStrategy -> PlayStrategy -> IO ()
match = step initialPosition

step :: Position VBoard -> PlayStrategy -> PlayStrategy -> IO ()
step p0 s1 s2 = do
  printf "* %s's turn\n" (show $ pPlayer p0)
  move <- s1 p0
  let p1 = doMove move p0
  case roundResult p1 of
    Winner p -> printf "* Game finished. Winner: %s\n" (show p)
    InProgress -> step p1 s2 s1

humanStrategy :: PlayStrategy
humanStrategy pos = do
  putStr "Enter your turn: "
  turnStr <- getLine
  case parseMove turnStr of
    Nothing -> do
      printf "Cannot parse move %s, please retype.\n" turnStr
      humanStrategy pos
    Just move -> return move

aiStrategy :: Algorithm -> Implementation -> Depth -> PlayStrategy
aiStrategy alg impl d pos = do
  let (aiPV, score) = search alg impl ThreatBasedEval pos d
      aiMove = head aiPV
  printf "AI evaluation (algo = %s, depth = %d): %d. PV: %s\n" (show alg) d score (show aiPV)
  printf "AI move: %s\n" (show aiMove)
  return aiMove

analysisStrategy :: Algorithm -> Implementation -> Depth -> PlayStrategy
analysisStrategy alg impl depth pos = do
  putStr "Enter your turn or depth to analyse: "
  turnStr <- getLine
  case parseMove turnStr of
    Nothing -> do
      case parseAnalyse turnStr of
        Nothing -> do
          printf "Cannot parse move %s, please retype.\n" turnStr
        Just newDepth -> do
          let (aiPV, score) = search alg impl ThreatBasedEval pos newDepth
          printf "Depth: %d. Score: %d. PV: %s\n" newDepth score (show aiPV)
      analysisStrategy alg impl depth pos

    Just move -> return move

  where
    parseAnalyse :: String -> Maybe Int
    parseAnalyse xs = case reads xs of
      [(i, "")] -> Just i
      _ -> Nothing
