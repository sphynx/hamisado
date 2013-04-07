import Control.DeepSeq
import Criterion.Main

import AI
import Analysis
import Types

losing :: Depth -> [Move]
losing = losingFirstMoves Negascout My

best :: Depth -> (PV, Int)
best = bestMove Negascout My

instance NFData Move where
  rnf (Move x y) = rnf x `seq` rnf y

main :: IO ()
main = defaultMain
 [ bgroup "losing"
    [ bench "d5" $ nf losing 5
    , bench "d7" $ nf losing 7
    , bench "d11" $ nf losing 11
    ]

 , bgroup "best"
    [ bench "d5" $ nf best 5
    , bench "d7" $ nf best 7
    ]
 ]
