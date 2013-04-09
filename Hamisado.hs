{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import AI
import Analysis
import Game
import Types
import Match

import System.Console.CmdArgs
import System.IO
import Text.Printf

data PlayMode = LosingMoves | BestMove | Play | Perft | Solve
  deriving (Show, Data, Typeable)

data Opponents = AIHuman | HumanAI | HumanHuman | AIAI | Analyse
  deriving (Show, Data, Typeable)

data Conf = Conf
  { algorithm      :: Algorithm
  , implementation :: Implementation
  , depth          :: Int
  , altDepth       :: Int
  , playMode       :: PlayMode
  , opponents      :: Opponents
  } deriving (Show, Data, Typeable)

conf :: Conf
conf = Conf
  { depth = 5                    &= help "Depth"
  , altDepth = 5                 &= help "Depth used for the second AI when tw oAIs are playing each other"
  , algorithm = Negascout        &= help "alphabeta | negascout | minimax | idalpha"
  , implementation = My          &= help "gametree | my | tzaar"
  , playMode = LosingMoves       &= help "losing | best | play | perft | solve"
  , opponents = AIHuman          &= help "AIHuman | HumanAI | HumanHuman | AIAI | Analyse"
  }
  &= summary "Hamisado AI system"
  &= program "Hamisado"

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
      putStr "Losing moves: "
      print $ losingFirstMoves algorithm implementation depth
    BestMove -> do
      putStr "Best move: "
      let (pv, score) = bestMove algorithm implementation depth
      printf "Score: %d, PV: %s\n" score (show pv)
    Play ->
      let ai = \d -> aiStrategy algorithm implementation d
          analysis = analysisStrategy algorithm implementation depth
          analyse = match analysis analysis
          human = humanStrategy
      in case opponents of
        AIHuman    -> match (ai depth) human
        HumanAI    -> match human (ai depth)
        HumanHuman -> match human human
        AIAI -> match (ai depth) (ai altDepth)
        Analyse     -> analyse
    Perft -> do
      -- You can learn more about Perft here:
      -- http://chessprogramming.wikispaces.com/Perft
      let leaves = map (reverse . pMoves) $ legalPositions depth initialPosition
      printf "Perft for depth=%d, total number of leaves=%d\n"
        depth (length leaves)
      mapM_ print leaves
    Solve ->
      printf "Solving result: %s\n" (show $ solve algorithm implementation depth)
