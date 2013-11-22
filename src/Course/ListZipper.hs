{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

zipper :: [a] -> a -> [a] -> ListZipper a
zipper l x r = ListZipper (listh l) x (listh r)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let zipper l x r = ListZipper (listh l) x (listh r)
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
--(<$>) :: (a -> b) -> f a -> f b
  (<$>) f (ListZipper l h r) = ListZipper (f <$> l) (f h) (f <$> r)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
--(<$>) :: (a -> b) -> f a -> f b
  (<$>) f a = 
    case a of
      IsNotZ -> IsNotZ
      IsZ x -> IsZ( f <$> x )

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- prop> xs == toListZ (fromList xs)
fromList ::
  List a
  -> MaybeListZipper a
fromList (h:.t) = 
  IsZ (ListZipper Nil h t)
fromList Nil = 
  IsNotZ

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> toOptional (fromOptional z) == z
toOptional ::
  MaybeListZipper a
  -> Optional (ListZipper a)
toOptional a =
  case a of
    IsZ p -> Full p
    IsNotZ -> Empty


fromOptional ::
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOptional Empty =
  IsNotZ
fromOptional (Full z) =
  IsZ z

asZipper ::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ . f)

(>$>)::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

(>->) ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>->) =
  asMaybeZipper

-- | Convert the given zipper back to a list.
toList ::
  ListZipper a
  -> List a
toList (ListZipper (a:.b) f r) =  toList . ListZipper b a $ f :. r
toList (ListZipper Nil f r) =  f :. r

  -- prepend head to r and f recursivly prepa

-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a
  -> List a
toListZ IsNotZ =
  Nil
toListZ (IsZ z) =
  toList z

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus ::
  (a -> a)
  -> ListZipper a
  -> ListZipper a
withFocus xs (ListZipper l f r) =
  ListZipper l (xs f) r

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus ::
  a
  -> ListZipper a
  -> ListZipper a
setFocus xs =
  withFocus (\_ -> xs)


-- A flipped infix alias for `setFocus`. This allows:
--
-- z := "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a
  -> a
  -> ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft ::
  ListZipper a
  -> Bool
hasLeft (ListZipper Nil _ _) = False
hasLeft (ListZipper (_:._) _ _) = True

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight ::
  ListZipper a
  -> Bool
hasRight (ListZipper _ _ Nil) = False
hasRight (ListZipper _ _ (_:._)) = True

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- prop> findLeft (const True) >-> fromList xs == fromList xs
--
-- prop> findLeft (const False) (zipper l x r) == IsNotZ
findLeft ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
findLeft _ (ListZipper Nil _ _) = IsNotZ
findLeft xs (ListZipper (h:.t) f r) =
  case xs f of
    True -> IsZ (ListZipper t h (f:.r))
    False -> findLeft xs (ListZipper t h (f:.r))

-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- prop> findRight (const True) >-> fromList xs == fromList xs
--
-- prop> findRight (const False) (zipper l x r) == IsNotZ
findRight ::
  (a -> Bool)
  -> ListZipper a
  -> MaybeListZipper a
findRight _ (ListZipper _ _ Nil) = IsNotZ
findRight xs (ListZipper l f (h:.t)) =
  case xs f of
    True -> IsZ (ListZipper l f (h:.t))
    False -> findRight xs (ListZipper (f:.l) h t)

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
-- CAUTION: This function is non-total, why?
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop ::
  ListZipper a
  -> ListZipper a
moveLeftLoop (ListZipper Nil f r) = 
  let (a:.b) = reverse (f:.r)
  in (ListZipper b a Nil)
moveLeftLoop (ListZipper (h:.t) f r) =
  (ListZipper t h (f:.r))

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop ::
  ListZipper a
  -> ListZipper a
moveRightLoop (ListZipper l f Nil) =
  let (a:.b) = reverse (f:.l)
  in (ListZipper Nil a b)
moveRightLoop (ListZipper l f (h:.t)) =
  (ListZipper (f:.l) h t)

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft ::
  ListZipper a
  -> MaybeListZipper a
moveLeft (ListZipper Nil _ _) = IsNotZ
moveLeft (ListZipper (a:.b) f r) = IsZ (ListZipper b a (f:.r))

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight ::
  ListZipper a
  -> MaybeListZipper a
moveRight (ListZipper _ _ Nil) = IsNotZ
moveRight (ListZipper l f (a:.b)) = IsZ (ListZipper (f:.l) a b)

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft ::
  ListZipper a
  -> MaybeListZipper a
swapLeft (ListZipper Nil _ _) = IsNotZ
swapLeft (ListZipper (a:.b) f r) = IsZ (ListZipper (f:.b) a r)

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight ::
  ListZipper a
  -> MaybeListZipper a
swapRight (ListZipper _ _ Nil) = IsNotZ
swapRight (ListZipper l f (a:.b)) = IsZ (ListZipper l a (f:.b))

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> dropLefts (zipper l x r) == zipper [] x r
dropLefts ::
  ListZipper a
  -> ListZipper a
dropLefts (ListZipper _ f r) = (ListZipper Nil f r)

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> dropRights (zipper l x r) == zipper l x []
dropRights ::
  ListZipper a
  -> ListZipper a
dropRights (ListZipper l f _) = (ListZipper l f Nil)

-- Move the focus left the given number of positions. If the value is negative, move right instead.
moveLeftN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
moveLeftN 0 z = IsZ z
moveLeftN i z = 
  if i >= 0 then 
    case moveLeft z of
      IsZ p -> moveLeftN (i - 1) p
      IsNotZ -> if i > 0 then IsNotZ else IsZ z
    else moveRightN (-1 * i) z


-- Move the focus right the given number of positions. If the value is negative, move left instead.
moveRightN ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
moveRightN 0 z = IsZ z
moveRightN i z = 
  if i >= 0 then 
    case moveRight z of
      IsZ p -> moveRightN (i - 1) p
      IsNotZ -> IsNotZ
    else moveLeftN (-1 * i) z

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveLeftN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveLeftN' intt zipp = 
  let moveLeftN'' o i z = let zzz' f g h = case f z of
                                             IsNotZ -> Left (g (o - i))
                                             IsZ p -> moveLeftN'' o (h i 1) p
                          in case i of
                               x | x > 0     -> zzz' moveLeft id (-)                              
                                 | x < 0     -> zzz' moveRight negate (+)
                                 | otherwise -> Right z
                      
  in moveLeftN'' intt intt zipp

xxx :: Int -> Int
xxx x | x > 0      = undefined
      | x < 0      = undefined
      | otherwise  = undefined

xxxx :: Int -> Int
xxxx x = 
  case x of
    z | z < 0 -> undefined
      | z > 0 -> undefined
      | otherwise -> undefined


-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' ::
  Int
  -> ListZipper a
  -> Either Int (ListZipper a)
moveRightN' i z = moveLeftN' (-i) z

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth ::
  Int
  -> ListZipper a
  -> MaybeListZipper a
nth intt zipp = 
  case moveLeftN' intt zipp of
    Left i -> moveRightN (intt - i) zipp
    Right _ -> moveRightN intt (start zipp) 

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- Full 3
--
-- prop> optional True (\i -> optional False (==z) (toOptional (nth i z))) (index z)
index ::
  ListZipper a
  -> Optional Int
index (ListZipper l _ _) =
  let count' i z = case z of
                    Nil      -> Empty
                    (_:.Nil) -> Full (i+1)
                    (_:.b)   -> count' (i+1) b
  in count' 0 l

-- | Move the focus to the end of the zipper.
-- CAUTION: This function is non-total, why?
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
end ::
  ListZipper a
  -> ListZipper a
end z@(ListZipper _ _ Nil) = z
end (ListZipper l f (a:.b)) = end (ListZipper (f:.l) a b)

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
start ::
  ListZipper a
  -> ListZipper a
start z@(ListZipper Nil _ _) = z
start (ListZipper (a:.b) f r) = start (ListZipper b a (f:.r))

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft ::
  ListZipper a
  -> MaybeListZipper a
deletePullLeft (ListZipper (a:.b) _ r) = IsZ (ListZipper b a r)
deletePullLeft (ListZipper Nil _ _) = IsNotZ

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight ::
  ListZipper a
  -> MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = IsNotZ
deletePullRight (ListZipper l f (a:.b)) = IsZ (ListZipper (f:.l) a b)

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft ::
  a
  -> ListZipper a
  -> ListZipper a
insertPushLeft z (ListZipper Nil f r) = (ListZipper (f:.Nil) z r)
insertPushLeft z (ListZipper l f r) = (ListZipper (f:.l) z r)

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight ::
  a
  -> ListZipper a
  -> ListZipper a
insertPushRight z (ListZipper l f Nil) = (ListZipper l z (f:.Nil))
insertPushRight z (ListZipper l f r) = (ListZipper l z (f:.r))

-- | Implement the `Apply` instance for `ListZipper`.
-- This implementation zips functions with values by function application.
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Apply ListZipper where
--(<$>) :: (a -> b) -> f a -> f b
--(<*>) :: (ListZipper) (a -> b) -> (ListZipper) a -> (ListZipper) b
  (<*>) (ListZipper a b c) (ListZipper l f r) = (ListZipper (zipWith ($) a l) (b f) (zipWith ($) c r))

-- | Implement the `Apply` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `<*>` for `ListZipper`.
instance Apply MaybeListZipper where
--(<*>) :: (MaybeListZipper) (a -> b) -> (MaybeListZipper) a -> (MaybeListZipper) b
  (<*>) IsNotZ _ = IsNotZ
  (<*>) _ IsNotZ = IsNotZ
  (<*>) (IsZ f) (IsZ a) = IsZ (f <*> a )

-- | Implement the `Applicative` instance for `ListZipper`.
-- This implementation produces an infinite list zipper (to both left and right).
--
-- /Tip:/ Use @Data.List#repeat@.
instance Applicative ListZipper where
  -- pure :: a -> f a
  pure a = (ListZipper (repeat a) a (repeat a))

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
instance Applicative MaybeListZipper where
  pure a = IsZ (pure a)

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @Data.List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
--(<<=) :: (f a -> b) -> f a -> f b
instance Extend ListZipper where
  (<<=) f a = 
    let z = f a
    in (ListZipper (unfoldr (\x -> 
      case moveLeft x of
        IsZ p -> Full (f p, p)
        IsNotZ -> Empty
      ) a) z (unfoldr (\x ->
      case moveRight x of
        IsZ p -> Full (f p, p)
        IsNotZ -> Empty
      ) a))

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure (ListZipper _ f _) = f

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
instance Traversable ListZipper where
  -- (a -> f b) -> t a -> f (t b)
  -- (a -> ListZipper b) -> t a -> ListZipper (t b)
  -- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- pure :: Applicative f => a -> f a
  -- (<$>) :: Functor f => (a -> f a) -> f a -> f (f a)
  traverse f a =  pure <$> f (copure a)

--  (a ->   b) -> f a -> f b -- fmap / <$>
--  (a -> f b) -> f a -> f b -- bind / flatMap / (=<<) / (>>=)
--f (a ->   b) -> f a -> f b -- apply / ap / <*>
--(f a ->   b) -> f a -> f b -- extend / cobind / <<=
--                  a -> f a -- pure / return
--                f a ->   a -- copure / extract


-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
instance Traversable MaybeListZipper where
  -- (a -> MaybeListZipper b) -> t a -> MaybeListZipper (t b)
  traverse f a = f `traverse` a

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"
