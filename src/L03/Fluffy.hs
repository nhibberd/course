module L03.Fluffy where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fluffy f where
  furry :: (a ->  b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
<<<<<<< HEAD:src/L04/Fluffy.hs
instance Fluffy List where
  -- furry :: (a -> b) -> List a -> List b
  furry =
    maap

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  -- furry :: (a -> b) -> Optional a -> Optional b
  furry f (Full a) = Full (f a)
  furry _ Empty = Empty

=======
instance Fluffy Id where
  furry =
    mapId

-- Exercise 2
-- Relative Difficulty: 2
instance Fluffy List where
  furry _ Nil    = Nil
  furry f (h:|t) = f h :| furry f t
>>>>>>> origin/nick:src/L03/Fluffy.hs

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Optional where
  furry _ Empty = Empty
  furry f (Full a) = Full (f a)

-- Exercise 4
-- Relative Difficulty: 3
instance Fluffy ((->) t) where
  furry f g =
    \x -> f (g x)

instance Fluffy ((->) t) where
  -- furry :: (a -> b) -> ((->) t a) -> ((->) t b)
  -- furry :: (a -> b) -> (t ->   a) -> (t ->   b)
  -- furry :: (a -> b) -> (t ->   a) ->  t ->   b 
  -- furry f g t = f (g t)
  furry = (.)


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
