module AITypes where

import Types

data Algorithm = Minimax | Negascout | AlphaBeta
data Implementation = My | GameTree | Tzaar
data Evaluation = SimpleEval | ThreatBasedEval

type PV = [Move]

