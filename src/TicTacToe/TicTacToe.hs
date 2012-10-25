module TicTacToe
(
  Board
, FinishedBoard
, Position(..)
, BoardLike(..)
, moveEmpty
, MoveResult(..)
, Draw(..)
, moveBoard
, wasX
, takeBackBoard
, takeBackFinished
, updateAt
, line
, whoWon
) where

import Data.List

data Player =
  X
  | O
  | E
  deriving (Eq, Show)

data Board =
  Board {
    tl :: Player
  , tm :: Player
  , tr :: Player
  , l :: Player
  , m :: Player
  , r :: Player
  , bl :: Player
  , bm :: Player
  , br :: Player
  , prev :: Position
  , prevs :: [Position]
  } deriving (Eq, Show)


data FinishedBoard =
  FinishedBoard Board deriving (Eq, Show)


data Position =
  TL
  | TM
  | TR
  | L
  | M
  | R
  | BL
  | BM
  | BR
  deriving (Eq, Enum, Show)

class BoardLike b where
  playerAt ::
    b
    -> Position
    -> Player

instance BoardLike Board where
  playerAt b p =
    case lookup p $ zip [TL .. BR] [tl, tm, tr, l, m, r, bl, bm, br] of
      Just y -> y b
      Nothing -> error "bad bad code"

instance BoardLike FinishedBoard where
  playerAt (FinishedBoard b) p =
    playerAt b p


moveEmpty ::
  Position
  -> Board
moveEmpty p = case p of
                  TL -> Board X E E E E E E E E TL []
                  TM -> Board E X E E E E E E E TM []
                  TR -> Board E E X E E E E E E TR []
                  L -> Board E E E X E E E E E L []
                  M -> Board E E E E X E E E E M []
                  R -> Board E E E E E X E E E R []
                  BL -> Board E E E E E E X E E BL []
                  BM -> Board E E E E E E E X E BM []
                  BR -> Board E E E E E E E E X BR []


data MoveResult =
  BoardResult Board
  | FinishedResult FinishedBoard
  | PositionOccupied
  deriving (Eq, Show)

data Draw =
  Draw 
  deriving (Eq, Show)

moveBoard ::
  Position
  -> Board
  -> MoveResult
moveBoard p b = let pl = playerAt b p
                in if pl == E
                      then let b' = updateAt b p (if wasX b then O else X)
                           in case whoWonBoard b' of
                                Left E -> BoardResult b'
                                _ -> FinishedResult (FinishedBoard b')
                      else PositionOccupied

wasX ::
  Board
  -> Bool
wasX b = (playerAt b (prev b)==X)


-- moveBoard :: ? -> Board -> Either Board FinishedBoard
-- moveEmpty :: ? -> Empty -> Board
-- takeBackBoard :: Board -> Either Empty Board
-- takeBackFinished :: Finished -> Board
-- whoWon :: Finished
-- playerAt 

takeBackBoard ::
  Board
  -> Maybe Board
takeBackBoard b = let x = (prevs b)
                  in case x of
                      [] -> Nothing
                      (h:t) -> let y = updateAt b (prev b) E
                               in Just y { prev = h, prevs = t }


takeBackFinished ::
  FinishedBoard
  -> Maybe Board
takeBackFinished  (FinishedBoard b) = let x = (prevs b)
                                     in case x of
                                      [] -> Nothing
                                      (h:t) -> let y = updateAt b (prev b) E
                                               in Just y { prev = h, prevs = t }

updateAt ::
  Board
  -> Position
  -> Player
  -> Board
updateAt b p pl =  case p of
          TL -> (b { tl = pl, prev = TL, prevs = prev b:prevs b })
          TM -> (b { tm = pl, prev = TM, prevs = prev b:prevs b  })
          TR -> (b { tr = pl, prev = TR, prevs = prev b:prevs b  })
          L -> (b { l = pl, prev = L, prevs = prev b:prevs b  })
          M -> (b { m = pl, prev = M, prevs = prev b:prevs b  })
          R -> (b { r = pl, prev = R, prevs = prev b:prevs b  })
          BL -> (b { bl = pl, prev = BL, prevs = prev b:prevs b  })
          BM -> (b { bm = pl, prev = BM, prevs = prev b:prevs b  })
          BR -> (b { br = pl, prev = BR, prevs = prev b:prevs b  })

line :: 
  Player
  -> Player
  -> Player
  -> Bool
line a b c = all (==a) [b,c]

--private
whoWonBoard ::
  Board
  -> Either Player Draw
whoWonBoard g = 
  let wins = [
               (TL, TM, TR)
             , (L, M, R)
             , (BL, BM, BR)
             , (TL, M, BR)
             , (TM, M, BM)
             , (TR, M, BL)
             , (TL, L, BL)
             , (TR, R, BR)
             ]
      positions = map (\(a, b, c) -> (playerAt g a, playerAt g b, playerAt g c)) wins
      found = find (\(a, b, c) -> line a b c) positions
  in case found of 
      Nothing -> if elem E (map (playerAt g) [TL .. BR]) then Right Draw else Left E
      Just (a, _, _) -> Left a

whoWon ::
  FinishedBoard
  -> Player
whoWon g = 
  let wins = [
               (TL, TM, TR)
             , (L, M, R)
             , (BL, BM, BR)
             , (TL, M, BR)
             , (TM, M, BM)
             , (TR, M, BL)
             , (TL, L, BL)
             , (TR, R, BR)
             ]
      positions = map (\(a, b, c) -> (playerAt g a, playerAt g b, playerAt g c)) wins
      found = find (\(a, b, c) -> line a b c) positions
  in case found of 
      Nothing -> E
      Just (a, _, _) -> a