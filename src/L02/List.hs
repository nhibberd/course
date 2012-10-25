-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Note the existence of the library function max :: Int -> Int -> Int which will help you with Exercise 9.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

-- TOTAL marks:    /66

module L02.List where

import Test.QuickCheck

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :| List t deriving Eq

-- Right-associative
infixr 5 :|

instance (Show t) => Show (List t) where
  show = show . foldRight (:) []

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :| t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :| t) = let b' = f b h in b' `seq` foldLeft f b' t

unfoldL :: (b -> Maybe (a, b)) -> b -> List a
unfoldL f b =
  case f b of
    Nothing -> Nil
    Just (a, b') -> a :| unfoldL f b'

toList :: [a] -> List a
toList = unfoldL (\q -> case q of
                          [] -> Nothing
                          (h:t) -> Just (h, t))
-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
headOr :: List a -> a -> a
headOr Nil a = a
headOr (x :| _) _ = x

{-
(:|) x y
x :| y
-}



-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
suum :: List Int -> Int
suum Nil = 0
suum (a :| b) = a + suum b

suuum :: List Int -> Int
suuum = foldLeft (+) 0

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
len :: List a -> Int
len = foldLeft (const . succ) 0


{-

B foldLeft(B => Int => B f, B b, List<Int> x) {
  B r = b;
  for(a : x) {
    r = f(r, a);
  }
  return r;
}

interface Func<T, U> { T apply(U u); }
<A, B, C> Func<A, C> compose(Func<B, C> f, Func<A, B> g) 
-}

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
maap :: (a -> b) -> List a -> List b
maap _ Nil = Nil
maap f (h :| t) = f h :| maap f t

maaap :: (a -> b) -> List a -> List b
maaap f = foldRight ((:|) . f)     Nil
--        foldRight ((:|) . ($ q)) Nil x

-- maap t a = 

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
fiilter :: (a -> Bool) -> List a -> List a
fiilter f = foldRight (\a -> if f a then (:|) a else id) Nil

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
append :: List a -> List a -> List a
append = flip (foldRight (:|))

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
flatten :: List (List a) -> List a
flatten = foldRight append Nil

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
flatMap :: (a -> List b) -> List a -> List b
flatMap f = foldRight (append . f) Nil
--flatMap f = flatten . maap f

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 2.0 marks
-- Elegance: 3.5 marks
-- Total: 9
seqf :: List (a -> b) -> a -> List b
seqf x q = foldRight ((:|) . ($ q)) Nil x

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
rev :: List a -> List a
rev Nil = Nil
rev (h :| t) = rev t `append` (h :| Nil)

rev2 :: List a -> List a -> List a
rev2 Nil acc = acc
rev2 (h:|t) acc = rev2 t (h:|acc)

revv :: List a -> List a
revv x = rev2 x Nil

revvv :: List a -> List a
revvv = foldLeft (flip (:|)) Nil

-- Exercise 10.1
-- How to produce arbitrary instances of List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (:|) Nil) arbitrary

-- END Exercises

unfoldR :: (b -> Maybe (a, b)) -> b -> List a
unfoldR f b = case f b of
                Nothing -> Nil
                Just (n, m) -> n :| unfoldR f m

foldRight2 :: ((a, b) -> b) -> (() -> b) -> List a -> b
foldRight2 _ b Nil      = b ()
foldRight2 f b (h :| t) = f (h, (foldRight2 f b t))


foldRight3 :: (Maybe (a, b) -> b) -> List a -> b
foldRight3 f Nil = f Nothing
foldRight3 f (h:|t) = f (Just (h, foldRight3 f t))

mapplusone :: Maybe Int -> Maybe Int
mapplusone x = case x of
                 Nothing -> Nothing
                 Just n -> Just (n+1)

cata :: x -> (a -> x) -> Maybe a -> x
cata n _ Nothing = n
cata _ j (Just a) = j a

taakeWhile :: (a -> Bool) -> List a -> List a
taakeWhile _ Nil = Nil
taakeWhile f (h:|t) = if f h then h:|taakeWhile f t else Nil

droopWhile :: (a -> Bool) -> List a -> List a
droopWhile _ Nil = Nil
droopWhile f (h:|t) = if f h then droopWhile f t else h:|t

spaan :: (a -> Bool) -> List a -> (List a, List a)
spaan f a = (taakeWhile f a, droopWhile f a)

grooup :: Eq a => List a -> List (List a)
grooup Nil = Nil
grooup (h:|t) = let (q, r) = spaan (==h) t
                in (h:|q):|grooup r

taails :: List a -> List (List a)
taails Nil = Nil:|Nil
taails a@(_:|t) = a:|taails t

fiind :: (a -> Bool) -> List a -> Maybe a
fiind _ Nil = Nothing
fiind f (h:|t) = if f h then Just h else fiind f t

ziip :: List a -> List b -> List (a, b)
ziip  _ Nil = Nil
ziip  Nil _ = Nil
ziip (h:|t) (c:|d) = (h,c):|ziip t d
