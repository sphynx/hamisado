{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall #-}

module Game
  ( start       -- starting position
  , doMove      -- do move{,s}
  , doMoves
  , genMoves    -- generate list of possible moves
  , next        -- generate list of moves applied to positions (1 ply or more)
  , forward
  , roundResult -- is terminal?


  -- Function exported for testing
  , between
  , playerPiecesCoords

  ) where

import Control.Arrow
import Control.Monad
import Data.Array
import Data.List
import Data.Maybe

import Types

board0 :: Board
board0 = listArray ((A,1), (H,8)) $ concat $ transpose $
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
  , rResult = InProgress
  }

doMoves :: [Move] -> Round -> Round
doMoves (m:ms) r =
  let r' = doMove m r
  in case rResult r' of
       InProgress -> doMoves ms r'
       _          -> r'
doMoves [] r = r

doMove :: Move -> Round -> Round
doMove m r@Round {..} = Round
  { rBoard  = updatedBoard
  , rMoves  = updatedMoves
  , rPlayer = alternate rPlayer
  , rResult = roundResult updatedBoard updatedMoves rPlayer
  }
  where updatedBoard = move m r
        updatedMoves = m : rMoves

-- M7, checking for the win.
roundResult :: Board -> [Move] -> Player -> RoundResult
roundResult board moves player -- player who moved in current round
  | reachedOpponentRow || isDeadlock = Winner player
  | otherwise                        = InProgress

  where
    reachedOpponentRow
      = any (`belongsToPlayer` player)
      $ rankPieces board
      $ opponentHomeRow player

    piece `belongsToPlayer` plr = pPlayer piece == plr

    isDeadlock = case moves of
      m:pm:_ -> isPass m && isPass pm
      _      -> False

move :: Move -> Round -> Board
move m@(Move from to) r
  | isPass m && (not $ hasAnyMoves from r) = rBoard r
  |
    -- M5 and M6. We should move if possible, we should pass
    -- otherwise. We check this here (not in isValidMove), because we
    -- use isValidMove to generate possible moves and we do not want
    -- cyclic dependencies.
    isPass m /= hasAnyMoves from r
    -- Pass and does not have moves - OK
    -- Normal move and have moves   - OK
    -- Pass, but have moves         - NO
    -- Normal move, but does not have moves - NO

    -- Move should be valid or should be a pass (which is not valid by default)
    -- This is confusing.
    && (isValidMove m r || isPass m) = --- FIXME: think about these checks, it's ugly now

  let b = rBoard r
      Field fromColor fromPiece = b ! from
      Field toColor   _         = b ! to
  in b // [ (to,   Field toColor   fromPiece)
          , (from, Field fromColor Nothing)
          ]

move m _ = error $ "Wrong or unsupported move: " ++ show m

isValidMove :: Move -> Round -> Bool
isValidMove (Move from to) Round{..} = isJust $ do
  let (from_x, from_y) = coord2int from
      (to_x,   to_y)   = coord2int to
      Field _ piece = rBoard ! from

  -- A piece exists in from field.
  p <- piece

  -- T1. A player may move only his/her pieces.
  guard $ pPlayer p == rPlayer

  -- T2. We may move only the piece which color is matching the color
  -- of "to" field of the previous move.
  unless (null rMoves) $
     guard $ colorOfToField rBoard (head rMoves) == pColor p

  -- M1. Move forward (straight or diagonal).
  guard $ forwardMovement from_x from_y to_x to_y rPlayer

  -- M2. No towers in between from and to fields.
  guard $ noPiecesInBetween from_x from_y to_x to_y rBoard

  -- M3. To field is empty.
  guard $ isNothing $ fPiece $ rBoard ! to


forwardMovement :: Int -> Int -> Int -> Int -> Player -> Bool
forwardMovement fx fy tx ty p = or
  -- Black forward straight
  [ ty > fy && tx == fx && p == Black -- up
  -- White forward straight
  , ty < fy && tx == fx && p == White -- down
  -- Black forward diagonal
  , ty > fy && (ty - fy) == abs (tx - fx) && p == Black -- up
  -- White forward diagonal
  , ty < fy && (fy - ty) == abs (tx - fx) && p == White -- down
  ]

noPiecesInBetween :: Int -> Int -> Int -> Int -> Board -> Bool
noPiecesInBetween fx fy tx ty b =
  all isNothing [fPiece $ b ! int2coord c | c <- between fx fy tx ty]

between :: (Enum t, Num t, Ord t) => t -> t -> t -> t -> [(t, t)]
between x1 y1 x2 y2
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

  where symmetric = between x2 y2 x1 y1

possiblePieceMoves :: Coord -> Round -> [Move]
possiblePieceMoves from r =
  [ m
  | c <- coords
  , let m = Move from c
  , isValidMove m r
  ]

hasAnyMoves :: Coord -> Round -> Bool
hasAnyMoves from = not . null . possiblePieceMoves from

genMoves :: Round -> [Move]
genMoves r@Round {..} =
  case rResult of
    InProgress ->
      let ms = concat [ possiblePieceMoves from r
                      | from <- playerPiecesCoords rPlayer rBoard
                      ]
      in if null ms then passMove else ms

    Winner _ -> [] --- Round has finished. Where to put refill?

  where
    passMove = case rMoves of
       m@Move{} : _ ->
         let col = colorOfToField rBoard m
             from = coordByColor col rPlayer rBoard
             in [Move from from]
       _ -> []


coordByColor :: Color -> Player -> Board -> Coord
coordByColor color p b = head
  [ coord
  | coord <- indices b
  , piece <- maybeToList $ fPiece $ b ! coord
  , pPlayer piece == p
  , pColor piece == color
  ]

playerPiecesCoords :: Player -> Board -> [Coord]
playerPiecesCoords p b =
  [ coord
  | coord <- indices b
  , piece <- maybeToList $ fPiece $ b ! coord
  , pPlayer piece == p
  ]

coord2int :: Coord -> (Int, Int)
coord2int = first (succ . fromEnum)

int2coord :: (Int, Int) -> Coord
int2coord = first (toEnum . pred)

colorOfToField :: Board -> Move -> Color
colorOfToField b (Move _ to) = fColor $ b ! to

isPass :: Move -> Bool
isPass (Move from to) = from == to

rankPieces :: Board -> Int -> [Piece]
rankPieces b r = catMaybes [fPiece $ b ! (f, r) | f <- [A .. H]]

homeRow :: Player -> Int
homeRow White = 8
homeRow Black = 1

opponentHomeRow :: Player -> Int
opponentHomeRow = homeRow . alternate

forward :: Int -> Round -> [Round]
forward 0 r = [r]
forward d r =
  [ doMove m r | m <- genMoves r ] >>= forward (d - 1)

next :: Round -> [Round]
next = forward 1
