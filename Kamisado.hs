{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Analysis
import Types
import System.IO
import Text.Printf

import System.Console.CmdArgs

data Algo = AlphaBeta | Negascout
  deriving (Show, Data, Typeable)

data PlayMode = LosingMoves | BestMove
  deriving (Show, Data, Typeable)

data Conf = Conf
  { depth     :: Int
  , algorithm :: Algo
  , playMode  :: PlayMode
  } deriving (Show, Data, Typeable)

conf :: Conf
conf = Conf
  { depth     = 5           &= help "Depth"
  , algorithm = Negascout   &= help "alphabeta | negascout"
  , playMode  = LosingMoves &= help "losing | best"
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
  Negascout -> error "N/A"

main :: IO ()
main = do
  Conf{..} <- cmdArgs conf
  hSetBuffering stdout NoBuffering
  putStrLn $ printf "Params: algo=%s, d=%d" (show algorithm) depth
  case playMode of
    LosingMoves -> do
      putStr "Moves:  "
      print $ losingMovesFunction algorithm depth
    BestMove -> do
      putStr "Best moves:  "
      let (moves, score) = bestMoveFunction algorithm depth
      printf "Score = %d, PV = %s\n" score (show $ reverse moves)
