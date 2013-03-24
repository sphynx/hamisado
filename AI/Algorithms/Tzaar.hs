{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing#-}


module AI.Algorithms.Tzaar
  ( alphabeta
  , negascout
  , negamax
  , Gametree(..)
  , Valuation
  , Valued(..)
  , valued
  , ($+)
  ) where

class Gametree p where
  children :: p -> [p]   -- list of move, position

  is_terminal :: p -> Bool
  is_terminal = null . children  -- default definition

-- | type for valuation functions
type Valuation a = a -> Int

-- | a pair of something with a strict integer valuation
-- supporting equality, ordering and limited arithmetic on valuation
data Valued a = Valued { value :: !Int, unvalued :: a } deriving Show

instance Functor Valued where
  fmap f (Valued v x) = Valued v (f x)

-- | apply a valuation
valued :: Valuation a -> a -> Valued a
valued f x = Valued (f x) x

-- | modify the valuation
revalue :: (Int -> Int) -> Valued a -> Valued a
revalue f (Valued v x) = Valued (f v) x

instance Eq (Valued a) where
  x == y = value x==value y

instance Ord (Valued a) where
  compare x y = compare (value x) (value y)


-- some instances of numeric type class (only negate and fromInteger)
instance Num (Valued a) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = revalue negate
  fromInteger n = valued (const (fromIntegral n)) undefined
  abs = undefined
  signum = undefined


-- | add a constant to a value
infix 6 $+
($+) :: Int -> Valued a -> Valued a
k $+ x = revalue (+k) x



-- | Naive negamax algorithm (no prunning)
-- wrapper
negamax :: Gametree p => Valuation p -> Int -> p -> Valued p
negamax node_value depth p = negamax' depth p
  where
    -- worker
    negamax' d p
      | d==0 || is_terminal p = valued node_value p
      | otherwise = negate $ minimum [negamax' d'  p' | p'<-children p]
        where d' = d-1

-- | Negamax with alpha-beta prunning
-- wrapper
alphabeta :: Gametree p => Valuation p -> Int -> p -> Valued p
alphabeta node_value depth p
  = let a = fromIntegral (minBound+1 :: Int)
        b = fromIntegral (maxBound :: Int)
    in alpha_beta' depth a b p
  where
    -- worker
    alpha_beta' d alfa beta p
        | d==0 || is_terminal p = valued node_value p
        | otherwise = cmx alfa (children p)
          where
            d' = d-1
            cmx alfa [] = alfa
            cmx alfa (p:ps)
              | a'>=beta = a'
              | otherwise = cmx (max a' alfa) ps
                where a' = negate $ alpha_beta' d' (negate beta) (negate alfa) p



-- | Negascout search
-- wrapper
negascout :: Gametree p => Valuation p -> Int -> p -> Valued p
negascout node_value depth p
  = let a = fromIntegral (minBound+1 :: Int)
        b = fromIntegral (maxBound :: Int)
    in negascout' node_value depth a b p

 where

  -- worker
  negascout' node_value d alfa beta p
    | d==0 || is_terminal p = valued node_value p
    | d==1  = valued (negate . node_value) p0       -- short-circuit for depth 1
    | b >= beta = b
    | otherwise = scout (max alfa b) b ps
      where
        d' = d-1
        ps = children p
        p0 = unvalued $ minimum $ map (valued node_value) ps
        -- p0 = estimate_best node_value ps  -- child with best static score
        b = negate $ negascout' node_value d' (negate beta) (negate alfa) p0 -- full search estimate

        scout _ !b [] = b
        scout !alfa !b (p:ps)
          | s>=beta = s
          | otherwise = scout alfa' b' ps
            where s = negate $ negascout' node_value d' (negate (1$+alfa)) (negate alfa) p
                  s' | s>alfa = negate $
                                negascout' node_value d' (negate beta) (negate alfa) p
                     | otherwise = s
                  alfa' = max alfa s'
                  b' = max b s'

