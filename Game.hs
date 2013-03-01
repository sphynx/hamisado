{-# LANGUAGE RecordWildCards #-}

module Game
  ( start       -- starting position
  , doMove      -- do move{,s}
  , doMoves
  , genMoves    -- generate list of possible moves
  , next        -- generate list of moves applied to positions (1 ply or more)
  , forward
  , roundResult -- is terminal?
  , isTerminal  -- is terminal?
  ) where

import Data.Array
import Data.List
import Data.Maybe

import Types

board0 :: Board
board0 = listArray ((1,1), (8,8)) $ concat $ transpose $
  [ [ filled minBound c | c <- colors ]
  , [ empty c | c <- [Purple, Brown, Yellow, Blue, Green, Pink, Orange, Red]]
  , [ empty c | c <- [Blue, Yellow, Brown, Purple, Red, Orange, Pink, Green]]
  , [ empty c | c <- [Yellow, Red, Green, Brown, Orange, Blue, Purple, Pink]]
  , [ empty c | c <- [Pink, Purple, Blue, Orange, Brown, Green, Red, Yellow]]
  , [ empty c | c <- [Green, Pink, Orange, Red, Purple, Brown, Yellow, Blue]]
  , [ empty c | c <- [Red, Orange, Pink, Green, Blue, Yellow, Brown, Purple]]
  , [ filled maxBound c | c <- reverse colors ]
  ] where
    empty :: Color -> Field
    empty = flip Field Nothing

    filled :: Player -> Color -> Field
    filled p c = Field c (Just $ Piece p c None)

start :: Round
start = Round
  { rBoard  = board0
  , rPlayer = Black
  , rMoves  = []
  }

doMoves :: [Move] -> Round -> Round
doMoves (m:ms) r =
  let r' = doMove m r
  in case roundResult r' of
       InProgress -> doMoves ms r'
       _          -> r'
doMoves [] r = r

doMove :: Move -> Round -> Round
doMove m r@Round {..} = Round
  { rBoard  = updateBoard m r
  , rMoves  = m : rMoves
  , rPlayer = alternate rPlayer
  }

roundResult :: Round -> RoundResult
roundResult Round{..}
  | reachedHomeRow || isDeadlock = Winner (alternate rPlayer)
  | otherwise                    = InProgress

  where
    reachedHomeRow = case rMoves of
      [] -> False
      Move _ to :_ -> snd to == homeRow rPlayer

    isDeadlock = case rMoves of
      m:pm:_ -> isPass m && isPass pm
      _      -> False

isTerminal :: Round -> Bool
isTerminal r =
  case roundResult r of
    Winner _ -> True
    _ -> False

updateBoard :: Move -> Round -> Board
updateBoard m r | isPass m   = rBoard r
updateBoard (Move from to) r =
  let b = rBoard r
      Field fromColor fromPiece = b ! from
      Field toColor   _         = b ! to
  in b // [ (to,   Field toColor   fromPiece)
          , (from, Field fromColor Nothing)
          ]

genForwardMoves :: Coord -> Player -> [Coord]
genForwardMoves (x,y) p = case p of
  Black ->
       [ (x,  y+i) | i <- [1 .. 8-y]]               -- straight up
    ++ [ (x-i,y+i) | i <- [1 .. min (x-1) (8-y)] ]  -- left up
    ++ [ (x+i,y+i) | i <- [1 .. min (8-x) (8-y)] ]  -- right up
  White ->
       [ (x,  y-i) | i <- [1 .. y-1] ]              -- straight down
    ++ [ (x-i,y-i) | i <- [1 .. min (x-1) (y-1)] ]  -- left down
    ++ [ (x+i,y-i) | i <- [1 .. min (8-x) (y-1)] ]  -- right down

noPiecesInBetween :: Coord -> Coord -> Board -> Bool
noPiecesInBetween from to b =
  all isNothing [fPiece $ b!c | c <- between from to]

between :: Coord -> Coord -> [Coord]
between (x1,y1) (x2,y2)
  -- equal?
  | x1 == x2 && y1 == y2                             = []
  -- on the same line?
  | abs(x1-y1) /= abs(x2-y2) && x1 /= x2 && y1 /= y2 = []

  -- vertical (2 cases)
  | x1 == x2 && y1 < y2  = [(x1, y) | y <- [y1+1 .. y2-1]]
  | x1 == x2 && y1 > y2  = symmetric

  -- horizontal (2 cases)
  | x1 < x2 && y1 == y2  = [(x, y1) | x <- [x1+1 .. x2-1]]
  | x1 > x2 && y1 == y2  = symmetric

  -- diagonal (4 cases)
  | x1 < x2 && y1 < y2   = [ (x,y) | x <- [x1+1 .. x2-1]
                           , let y = y1 + x - x1
                           ]
  | x1 < x2 && y1 > y2   = [ (x,y) | x <- [x1+1 .. x2-1]
                           , let y = y1 - x + x1
                           ]
  | x1 > x2              = symmetric

  | otherwise = error "Should not happen"

  where symmetric = between (x2,y2) (x1,y1)

possiblePieceMoves :: Coord -> Round -> [Move]
possiblePieceMoves from Round{..} =
  [ Move from to
  | to <- genForwardMoves from rPlayer
  , noPiecesInBetween from to rBoard
  , isNothing $ fPiece $ rBoard ! to
  ]

initialFroms :: Player -> [Coord]
initialFroms Black = [(x,1) | x <- [1..8]]
initialFroms White = [(x,8) | x <- [1..8]]

requiredFrom :: Move -> Round -> Coord
requiredFrom lastMove Round{..} =
  coordByColor (colorOfToField rBoard lastMove) rPlayer rBoard

requiredFroms :: Round -> [Coord]
requiredFroms r = case rMoves r of
  []  -> initialFroms (rPlayer r)
  m:_ -> [ requiredFrom m r ]

passMove :: Round -> [Move]
passMove r = case rMoves r of
  []  -> []
  m:_ -> let from = requiredFrom m r
          in [Move from from]

genMoves :: Round -> [Move]
genMoves r | Winner _ <- roundResult r = []
genMoves r  = result where
  moves = concat [ possiblePieceMoves from r
                 | from <- requiredFroms r
                 ]
  result | null moves = passMove r
         | otherwise  = moves

coordByColor :: Color -> Player -> Board -> Coord
coordByColor color p b = head
  [ coord
  | coord <- indices b
  , Just piece <- [fPiece $ b ! coord]
  , pPlayer piece == p
  , pColor piece == color
  ]

colorOfToField :: Board -> Move -> Color
colorOfToField b (Move _ to) = fColor $ b ! to

isPass :: Move -> Bool
isPass (Move from to) = from == to

homeRow :: Player -> Int
homeRow Black = 1
homeRow White = 8

forward :: Int -> Round -> [Round]
forward 0 r = [r]
forward d r =
  [ doMove m r | m <- genMoves r ] >>= forward (d - 1)

next :: Round -> [Round]
next = forward 1
