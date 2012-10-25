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
  } deriving (Eq, Show)

data Game =
  Game Board [Position]
  deriving (Eq, Show)

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

playerAt ::
  Position
  -> Game
  -> Player
playerAt p (Game b _) =
  case lookup p $ zip [TL .. BR] [tl, tm, tr, l, m, r, bl, bm, br] of
    Just y -> y b
    Nothing -> error "bad bad code"

emptyGame ::
  Game
emptyGame =
  Game (Board E E E E E E E E E) []

wasX ::
  Game
  -> Bool
wasX g@(Game _ (h:_)) =
  playerAt h g == X
wasX (Game _ []) = 
  True
  
wasO ::
  Game
  -> Bool
wasO =
  not . wasX


updateAt ::
  Game
  -> Position
  -> Player
  -> Game
updateAt g@(Game b l) p pl = 
  Game (case p of
          TL -> (b { tl = pl })
          TM -> (b { tm = pl })
          TR -> (b { tr = pl })
          L -> (b { l = pl })
          M -> (b { m = pl })
          R -> (b { r = pl })
          BL -> (b { bl = pl })
          BM -> (b { bm = pl })
          BR -> (b { br = pl })
          ) (p:l)

move ::
  Game
  -> Position
  -> Maybe Game
move g@(Game b l) p = let pl = playerAt p g
                      in if pl == E
                            then Just $ updateAt g p (if wasX g then O else X) --updateAt
                            else Nothing

line :: 
  Player
  -> Player
  -> Player
  -> Bool
line a b c = all (==a) [b,c]

whosWon ::
  Game
  -> Player
whosWon g@(Game b l) =
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
      positions = map (\(a, b, c) -> (playerAt a g, playerAt b g, playerAt c g)) wins
      found = find (\(a, b, c) -> line a b c) positions
  in case found of 
      Nothing -> E
      Just (a, _, _) -> a
