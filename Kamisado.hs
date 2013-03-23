{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import AI
import Analysis
import Game
import Types

import Control.Applicative
import System.Console.CmdArgs
import System.IO
import Text.Printf

data PlayMode = LosingMoves | BestMove | Play
  deriving (Show, Data, Typeable)

data AIPlayer = First | Second
  deriving (Show, Data, Typeable)

data Conf = Conf
  { algorithm      :: Algorithm
  , implementation :: Implementation
  , depth          :: Int
  , playMode       :: PlayMode
  , turn           :: AIPlayer
  } deriving (Show, Data, Typeable)

conf :: Conf
conf = Conf
  { depth = 5                    &= help "Depth"
  , algorithm = AlphaBeta        &= help "alphabeta | negascout"
  , implementation = GameTree    &= help "gametree | my | tzaar"
  , playMode = LosingMoves       &= help "losing | best | play"
  , turn = First                 &= help "first | second"
  }
  &= summary "Kamisado AI system"
  &= program "Kamisado"

losingMovesFunction :: Conf -> [Move]
losingMovesFunction  =
  losingFirstMoves <$> algorithm <*> implementation <*> depth

bestMoveFunction :: Conf -> (PV, Int)
bestMoveFunction =
  bestMove <$> algorithm <*> implementation <*> depth

currentMoveFunction :: Conf -> Round -> (PV, Int)
currentMoveFunction c r =
  search (algorithm c) (implementation c) ThreatBasedEval r (depth c)

humanMoves :: Conf -> Round -> IO ()
humanMoves c r0 = do
  putStr "Enter your turn: "
  turnStr <- getLine

  let humanMove = parseMove turnStr
      r1 = doMove humanMove r0

  case roundResult r1 of
    Winner p -> printf "Game finished. Winner: %s\n" (show p)
    InProgress -> aiMoves c r1

aiMoves :: Conf -> Round -> IO ()
aiMoves c r0 = do
  let (aiPV, score) = currentMoveFunction c r0
      aiMove = head aiPV
      r1 = doMove aiMove r0

  printf "Score: %d. PV: %s\n" score (show aiPV)
  printf "AI move: %s\n" (show aiMove)

  case roundResult r1 of
    Winner p -> printf "Game finished. Winner: %s\n" (show p)
    InProgress -> humanMoves c r1


dumpOptions :: Conf -> String
dumpOptions c =
  printf "Params: mode=%s, algo=%s, implementation=%s, depth=%d"
    (show $ playMode c)
    (show $ algorithm c)
    (show $ implementation c)
    (depth c)

main :: IO ()
main = do
  c@Conf{..} <- cmdArgs conf
  hSetBuffering stdout NoBuffering
  putStrLn $ dumpOptions c
  case playMode of
    LosingMoves -> do
      putStr "Moves:  "
      print $ losingMovesFunction c
    BestMove -> do
      putStr "Best moves:  "
      let (pv, score) = bestMoveFunction c
      printf "Score: %d, PV: %s\n" score (show pv)
    Play ->
      case turn of
        First  -> aiMoves c start
        Second -> humanMoves c start
