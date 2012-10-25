module L04.Fluffy where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Fluffy f where
  furry :: (a ->  b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
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


-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry =
    error "todo"

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
