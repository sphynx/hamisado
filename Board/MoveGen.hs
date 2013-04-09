module Board.MoveGen where

import Data.Bits
import Data.Char
import Data.Word
import Numeric
import Utils
import Text.Printf


import qualified Data.Vector.Unboxed as V

{-
  noWe         nort         noEa
          +7    +8    +9
              \  |  /
  west    -1 <-  0 -> +1    east
              /  |  \
          -9    -8    -7
  soWe         sout         soEa

-}


{-

Board indexing:

y (row/rank)

8 | 56 57 58 59 60 61 62 63
7 | 48 49 50 51 52 53 54 55
6 | 40 41 42 43 44 45 46 47
5 | 32 33 34 35 36 37 38 39
4 | 24 25 26 27 28 29 30 31
3 | 16 17 18 19 20 21 22 23
2 |  8  9 10 11 12 13 14 15
1 |  0  1  2  3  4  5  6  7
----------------------------
  |  1  2  3  4  5  6  7  8   -- x (col/file)
  |  A  B  C  D  E  F  G  H

,
  0x02824222120a0700


-}

index64 :: V.Vector Int
index64 = V.fromList [
    0, 47,  1, 56, 48, 27,  2, 60,
   57, 49, 41, 37, 28, 16,  3, 61,
   54, 58, 35, 52, 50, 42, 21, 44,
   38, 32, 29, 23, 17, 11,  4, 62,
   46, 55, 26, 59, 40, 36, 15, 53,
   34, 51, 20, 43, 31, 22, 10, 45,
   25, 39, 14, 33, 19, 30,  9, 24,
   13, 18,  8, 12,  7,  6,  5, 63
   ]

-- /**
--  * bitScanForward
--  * @author Kim Walisch (2012)
--  * @param bb bitboard to scan
--  * @precondition bb != 0
--  * @return index (0..63) of least significant one bit
--  */
-- int bitScanForward(U64 bb) {
--    const U64 debruijn64 = C64(0x03f79d71b4cb0a89);
--    assert (bb != 0);
--    return index64[((bb ^ (bb-1)) * debruijn64) >> 58];
-- }

bitScanForward :: Word64 -> Int
bitScanForward bb =
  let debruijn64 = 0x03f79d71b4cb0a89
      ix = ((bb `xor` (bb-1)) * debruijn64) `shiftR` 58
  in V.unsafeIndex index64 (fromIntegral ix)


moves :: Word64 -> Square -> Dir -> Word64
moves occ s d =
  let aix = attackIndex s d
      attack = V.unsafeIndex attacks aix
      obstacles = occ .&. attack
      firstObstacle = bitScanForward obstacles
      aix2 = attackIndex firstObstacle d
      attack2 = V.unsafeIndex attacks aix2
  in attack `xor` attack2




occupancy :: Word64
occupancy = 0x00000000FF000000


ls1b :: Word64 -> Word64
ls1b x = x .&. (-x)


attacks :: V.Vector Word64
attacks = V.fromList
 [0x8141211109050300,
  0x02824222120a0700,
  0x0404844424150e00,
  0x08080888492a1c00,
  0x1010101192543800,
  0x2020212224a87000,
  0x404142444850e000,
  0x8182848890a0c000,
  0x4121110905030000,
  0x824222120a070000,
  0x04844424150e0000,
  0x080888492a1c0000,
  0x1010119254380000,
  0x20212224a8700000,
  0x4142444850e00000,
  0x82848890a0c00000,
  0x2111090503000000,
  0x4222120a07000000,
  0x844424150e000000,
  0x0888492a1c000000,
  0x1011925438000000,
  0x212224a870000000,
  0x42444850e0000000,
  0x848890a0c0000000,
  0x1109050300000000,
  0x22120a0700000000,
  0x4424150e00000000,
  0x88492a1c00000000,
  0x1192543800000000,
  0x2224a87000000000,
  0x444850e000000000,
  0x8890a0c000000000,
  0x0905030000000000,
  0x120a070000000000,
  0x24150e0000000000,
  0x492a1c0000000000,
  0x9254380000000000,
  0x24a8700000000000,
  0x4850e00000000000,
  0x90a0c00000000000,
  0x0503000000000000,
  0x0a07000000000000,
  0x150e000000000000,
  0x2a1c000000000000,
  0x5438000000000000,
  0xa870000000000000,
  0x50e0000000000000,
  0xa0c0000000000000,
  0x0300000000000000,
  0x0700000000000000,
  0x0e00000000000000,
  0x1c00000000000000,
  0x3800000000000000,
  0x7000000000000000,
  0xe000000000000000,
  0xc000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000000,
  0x0000000000000003,
  0x0000000000000007,
  0x000000000000000e,
  0x000000000000001c,
  0x0000000000000038,
  0x0000000000000070,
  0x00000000000000e0,
  0x00000000000000c0,
  0x0000000000000305,
  0x000000000000070a,
  0x0000000000000e15,
  0x0000000000001c2a,
  0x0000000000003854,
  0x00000000000070a8,
  0x000000000000e050,
  0x000000000000c0a0,
  0x0000000000030509,
  0x0000000000070a12,
  0x00000000000e1524,
  0x00000000001c2a49,
  0x0000000000385492,
  0x000000000070a824,
  0x0000000000e05048,
  0x0000000000c0a090,
  0x0000000003050911,
  0x00000000070a1222,
  0x000000000e152444,
  0x000000001c2a4988,
  0x0000000038549211,
  0x0000000070a82422,
  0x00000000e0504844,
  0x00000000c0a09088,
  0x0000000305091121,
  0x000000070a122242,
  0x0000000e15244484,
  0x0000001c2a498808,
  0x0000003854921110,
  0x00000070a8242221,
  0x000000e050484442,
  0x000000c0a0908884,
  0x0000030509112141,
  0x0000070a12224282,
  0x00000e1524448404,
  0x00001c2a49880808,
  0x0000385492111010,
  0x000070a824222120,
  0x0000e05048444241,
  0x0000c0a090888482,
  0x0003050911214181,
  0x00070a1222428202,
  0x000e152444840404,
  0x001c2a4988080808,
  0x0038549211101010,
  0x0070a82422212020,
  0x00e0504844424140,
  0x00c0a09088848281]


display :: Word64 -> IO ()
display x =
  mapM_ (putStrLn . reverse) $
  groupIn 8 $
  printf "%064s" $
  showIntAtBase 2 intToDigit x ""


attackIndex :: Square -> Dir -> Int
attackIndex s d = if d == Pos then s else 64+s

printTable = mapM_ putStrLn
  [ f
  | d <- [Pos, Neg]
  , s <- [0..63]
  , let w64 = t s d
  , let ix = attackIndex s d
  , let f = printf "(%d, 0x%016x)," ix w64
  ]

ix :: (Int, Int) -> Int
ix (x,y) = (y-1) * 8 + (x-1)

cix :: Int -> (Int, Int)
cix i = let (q,r) = i `quotRem` 8
        in (r+1, q+1)

data Dir = Pos | Neg deriving (Eq)
type Square = Int


t :: Square -> Dir -> Word64
t s d = ray2word64 $ attackRay s d

ray2word64 :: [Square] -> Word64
ray2word64 = foldl setBit 0

-- True = up
attackRay :: Square -> Dir -> [Square]
attackRay z p =
  let (x,y) = cix z
  in map ix $ case p of
  Pos ->
       [ (x-i,y+i) | i <- [1 .. min (x-1) (8-y)] ]  -- left up
    ++ [ (x,  y+i) | i <- [1 .. 8-y] ]              -- straight up
    ++ [ (x+i,y+i) | i <- [1 .. min (8-x) (8-y)] ]  -- right up
  Neg ->
       [ (x-i,y-i) | i <- [1 .. min (x-1) (y-1)] ]  -- left down
    ++ [ (x,  y-i) | i <- [1 .. y-1] ]              -- straight down
    ++ [ (x+i,y-i) | i <- [1 .. min (8-x) (y-1)] ]  -- right down


ray :: (Int -> Int) -> Bool -> Int -> [Int]
ray dir increasing square =
  let inside = if increasing then (<64) else (>=0)
  in case takeWhile inside $ iterate dir square of
    [] -> []
    squares -> tail squares


north = ray (+8) True
nw = ray (+7) True
ne = ray (+9) True

south = ray (\x -> x - 8) False
sw = ray (\x -> x - 9) False
se = ray (\x -> x - 7) False


