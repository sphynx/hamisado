{-# LANGUAGE DeriveDataTypeable #-}

module AI.Types where

import Types
import Data.Data

data Algorithm =
  Minimax | Negascout | AlphaBeta  deriving (Show, Data, Typeable)
data Implementation = My | GameTree | Tzaar deriving (Show, Data, Typeable)
data Evaluation = SimpleEval | ThreatBasedEval deriving (Show, Data, Typeable)

type PV = [Move]

