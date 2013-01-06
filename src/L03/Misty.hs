module L03.Misty where

import L01.Id
import L01.Optional
import L02.List


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)
<<<<<<< HEAD:src/L04/Misty.hs
    -- f :: a -> b
    -- x :: m a
    -- a :: a
    -- ? :: m b
=======
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 5
-- Relative Difficulty: 1
instance Misty Id where
  banana = flip bindId
  unicorn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Misty List where
<<<<<<< HEAD:src/L04/Misty.hs
  -- (a -> List b) -> List a -> List b
  banana = flatMap
  -- a -> List a
  unicorn a = a :| Nil
=======
  banana _ Nil    = Nil
  banana f (h:|t) = f h `append` banana f t
  unicorn = flip (:|) Nil
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 7
-- Relative Difficulty: 2
instance Misty Optional where
<<<<<<< HEAD:src/L04/Misty.hs
  -- (a -> Optional b) -> Optional a -> Optional b
  banana _ Empty = Empty
  banana f (Full a) = f a
  -- a -> Optional a
=======
  banana _ Empty    = Empty
  banana f (Full a) = f a
>>>>>>> origin/nick:src/L03/Misty.hs
  unicorn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Misty ((->) t) where
  banana f g = \x -> f (g x) x
  unicorn x = \_ -> x

<<<<<<< HEAD:src/L04/Misty.hs
instance Misty ((->) t) where
  -- (a -> m b) -> m a -> m b
  -- (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
  --     (a -> t -> b) -> (t -> a) -> t -> b
  banana f                g           t =  f (g t) t
  -- a -> ((->) t a)
  -- a -> (t -> a)
  -- a -> t -> a
  unicorn a _ = a

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean  = banana id
=======
-- Exercise 9
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 10
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
<<<<<<< HEAD:src/L04/Misty.hs
sausage [] = unicorn []
sausage (h:t) = banana (\a -> 
                banana (\as -> 
                unicorn ( a:as )) 
                  (sausage t))
                    h

sausage2 :: Monad m => [m a] -> m [a]                    
sausage2 [] = return []
sausage2 (h:t) = h          >>= \a -> 
                 sausage2 t >>= \as -> 
                 return (a:as)

sausage3 :: Monad m => [m a] -> m [a]                    
sausage3 [] = return []
sausage3 (h:t) = do a  <- h
                    as <- sausage3 t
                    return (a:as)

=======
sausage []    = unicorn []
sausage (h:t) = (\h' -> (\t' -> h':t') `furry'` sausage t) `banana` h
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 11
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
<<<<<<< HEAD:src/L04/Misty.hs
moppy f = sausage . furry' f
=======
moppy f a = sausage (furry' f a)
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 12
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar n a = sausage (replicate n a)

-- Exercise 13
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering _ []    = unicorn []
filtering f (h:t) = (\g -> (\gs -> if g then h:gs else gs) `furry'` filtering f t) `banana` f h

-- Exercise 14
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
<<<<<<< HEAD:src/L04/Misty.hs
apple f a = banana (\k -> furry' k a) f
=======
apple f a = (\f' -> (\a' -> f' a') `furry'` a) `banana` f
>>>>>>> origin/nick:src/L03/Misty.hs

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 f a = apple (f `furry'` a)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f a b = apple ((f `furry'` a) `apple` b)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 f a b c =  apple (((f `furry' ` a) `apple` b) `apple` c)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
