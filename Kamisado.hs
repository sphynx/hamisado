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

data Conf = Conf
  { depth :: Int
  , algorithm :: Algo
  } deriving (Show, Data, Typeable)

conf :: Conf
conf = Conf
  { depth = 5 &= help "Depth"
  , algorithm = Negascout &= help "alphabeta | negascout"
  }
  &= summary "Kamisado analyzer"
  &= program "Kamisado"

solveFunction :: Algo -> (Int -> [Move])
solveFunction a = case a of
  AlphaBeta -> losingFirstMovesAB
  Negascout -> losingFirstMovesNS

main :: IO ()
main = do
  Conf{..} <- cmdArgs conf
  let f = solveFunction algorithm
  hSetBuffering stdout NoBuffering
  putStrLn $ printf "Params: algo=%s, d=%d" (show algorithm) depth
  putStr "Moves:  "
  print $ f depth
