module AI
 ( search
 , module AI.Types
 ) where

import Types
import AI.Types
import Text.Printf

import qualified AI.API.My as My
import qualified AI.API.Tzaar as Tzaar
import qualified AI.API.GameTree as GameTree

search :: Board b => Algorithm -> Implementation -> Evaluation -> Position b -> Depth -> (PV, Score)
search Minimax My = My.minimax
search AlphaBeta My = My.alphabeta
search Negascout My = My.negascout
search Minimax GameTree = GameTree.minimax
search AlphaBeta GameTree = GameTree.alphabeta
search Negascout GameTree = GameTree.negascout
search Minimax Tzaar = Tzaar.minimax
search AlphaBeta Tzaar = Tzaar.alphabeta
search Negascout Tzaar = Tzaar.negascout
search a i = error $ printf "Unsupported algorithm-implementation pair: (%s, %s)"
                     (show a) (show i)
