module AI where

import Game
import AITypes
import Types

import qualified AI.MySearchAPI as My
import qualified AI.TzaarSearchAPI as Tzaar
import qualified AI.GameTreeSearchAPI as GameTree

search :: Algorithm -> Implementation -> Evaluation -> Round -> Depth -> (PV, Score)
search AlphaBeta My = My.alphaBeta
search AlphaBeta GameTree = GameTree.alphaBeta
search AlphaBeta Tzaar = Tzaar.alphaBeta
search Negascout My = My.negascout
search Negascout GameTree = GameTree.negascout
search Negascout Tzaar = Tzaar.negascout




