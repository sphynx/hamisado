{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Analysis
import Types
import Game
import Search
import System.IO
import Text.Printf

import System.Console.CmdArgs

data Algo = AlphaBeta | Negascout
  deriving (Show, Data, Typeable)

data PlayMode = LosingMoves | BestMove | Play
  deriving (Show, Data, Typeable)

data AIPlayer = First | Second
  deriving (Show, Data, Typeable)

data Conf = Conf
  { depth     :: Int
  , algorithm :: Algo
  , playMode  :: PlayMode
  , turn      :: AIPlayer
  } deriving (Show, Data, Typeable)

conf :: Conf
conf = Conf
  { depth     = 5           &= help "Depth"
  , algorithm = Negascout   &= help "alphabeta | negascout"
  , playMode  = LosingMoves &= help "losing | best | play"
  , turn      = First       &= help "first | second"
  }
  &= summary "Kamisado analyzer"
  &= program "Kamisado"

losingMovesFunction :: Algo -> Int -> [Move]
losingMovesFunction a = case a of
  AlphaBeta -> losingFirstMovesAB
  Negascout -> losingFirstMovesNS

bestMoveFunction :: Algo -> Int -> ([Move], Int)
bestMoveFunction a = case a of
  AlphaBeta -> bestMovesAB
  Negascout -> bestMovesNS

currentMoveFunction :: Algo -> Round -> Int -> ([Move], Int)
currentMoveFunction a = case a of
  AlphaBeta -> alphaBeta2
  Negascout -> negaScout2

main :: IO ()
main = do
  c@Conf{..} <- cmdArgs conf
  hSetBuffering stdout NoBuffering
  putStrLn $ printf "Params: algo=%s, d=%d, playmode=%s" (show algorithm) depth (show playMode)
  case playMode of
    LosingMoves -> do
      putStr "Moves:  "
      print $ losingMovesFunction algorithm depth
    BestMove -> do
      putStr "Best moves:  "
      let (pv, score) = bestMoveFunction algorithm depth
      printf "Score: %d, PV: %s\n" score (show pv)
    Play ->
      case turn of
        First  -> aiMoves c start
        Second -> humanMoves c start


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
  let (aiPV, score) = currentMoveFunction (algorithm c) r0 (depth c)
      aiMove = head aiPV
      r1 = doMove aiMove r0

  printf "Score: %d. PV: %s\n" score (show aiPV)
  printf "AI move: %s\n" (show aiMove)

  case roundResult r1 of
    Winner p -> printf "Game finished. Winner: %s\n" (show p)
    InProgress -> humanMoves c r1

