{-# LANGUAGE NoImplicitPrelude #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
instance Functor (State s) where
  f <$> sa =
    State (\s -> case (runState sa s) of
              (a',s') -> (f a', s'))

date Hole = Hole

-- | Implement the `Apply` instance for `State s`.
instance Apply (State s) where
  -- (<*>) :: f (a -> b) -> f a -> f b
  f <*> sa =
    State (\s -> case (runState sa s) of
              (a', s') -> case (runState f s') of
                (f', s'') -> (f' a', s''))

-- | Implement the `Applicative` instance for `State s`.
instance Applicative (State s) where
  pure a = State (\s -> (a,s))


-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
instance Bind (State s) where
  -- (=<<) :: f a -> (a -> f b) -> f b
  f =<< sa =
    State (\s -> case (runState sa s) of
              (a', s') -> runState (f a') s')

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec sa s =
  case runState sa s of
    (_, s') -> s'

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval sa s =
  case runState sa s of
    (a', _) -> a'

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get =
  State(\s -> (s,s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s =
  State (\_ -> ((), s))

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM f (h:.t) =
  f h >>= (\a -> case a of
            True -> pure (Full h)
            False -> findM f t)

-- f Bool -> f (Optional a)
-- f Bool -> Bool

-- f a -> f (\a -> case)

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = foldRight (:) [] xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat =
  error "todo"

-- | Remove all elements in a `List` that fail a given predicate.
-- However, while performing the filter, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#filter
-- where the effect appears in every return position:
--   filter ::  (a ->   Bool) -> List a ->    List a
--   filterM :: (a -> f Bool) -> List a -> f (List a)
--
-- >>> let p x = Full (x `mod` 2 == 0); xs = listh [1..10] in filterM p xs
-- Full [2,4,6,8,10]
--
-- >>> let p x = if x `mod` 2 == 0 then Full True else Empty; xs = listh [1..10] in filterM p xs
-- Empty
filterM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM =
  error "todo"

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filterM` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct =
  error "todo"

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce =
  error "todo"

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `flatten` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Data.Foldable#elem@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  error "todo"
