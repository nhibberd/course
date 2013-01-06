module L05.Parser where

import Control.Applicative
import Data.Char
import L01.Validation
import L05.Person


type Input = String
type Result x = Validation (Input, x)

data Parser a = P {
  parse :: Input -> Result a
}

-- Exercise 1
-- Return a parser that always succeeds
-- with the given value and consumes no input.
valueParser :: a -> Parser a
<<<<<<< HEAD:src/L03/Parser.hs
valueParser a = P $ \input -> Value (input, a)
=======
valueParser a = P (\i -> Value (i, a))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 2
-- Return a parser that always fails
-- with the given error.
failed :: Err -> Parser a
<<<<<<< HEAD:src/L03/Parser.hs
failed e = P $ \_ -> Error e
=======
failed e = P (\_ -> Error e)
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 3
-- Return a parser that succeeds with a character
-- off the input or fails with an error if the input is empty.
character :: Parser Char
<<<<<<< HEAD:src/L03/Parser.hs
character = P $ \input -> case input of
                            [] -> Error "eof" -- is no character
                            (h:t) -> Value (t, h)
=======
character = P (\s -> case s of [] -> Error "Unexpected end of stream"
                               (c:r) -> Value (r, c))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 4
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--   * if that parser fails with an error the returned parser fails with that error.
bindParser :: Parser a -> (a -> Parser b) -> Parser b
<<<<<<< HEAD:src/L03/Parser.hs
bindParser (P p) f = P $ \input -> case p input of
                                     Error e -> Error e
                                     Value (rest, a) -> parse (f a) rest
=======
bindParser (P p) f = P (\s -> case p s of Value (r, c) -> parse (f c) r
                                          Error e -> Error e)
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 5
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--   * if that parser fails with an error the returned parser fails with that error.
-- ~~~ This function should call bindParser. ~~~
(>>>) :: Parser a -> Parser b -> Parser b
<<<<<<< HEAD:src/L03/Parser.hs
p >>> q = bindParser p (const q)
=======
p >>> q = bindParser p (\_ -> q)
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 6
-- Return a parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
(|||) :: Parser a -> Parser a -> Parser a
<<<<<<< HEAD:src/L03/Parser.hs
P p1 ||| P p2 = P $ \input -> case p1 input of
                                Error _ -> p2 input
                                Value x -> Value x
=======
P p1 ||| P p2 = P (\s -> case p1 s of v@(Value _) -> v
                                      Error _ -> p2 s)
>>>>>>> origin/nick:src/L05/Parser.hs

infixl 3 |||

-- Exercise 7
-- Return a parser that continues producing a list of values from the given parser.
-- ~~~ Use many1, valueParser and (|||). ~~~
list :: Parser a -> Parser [a]
<<<<<<< HEAD:src/L03/Parser.hs
list a = many1 a ||| valueParser []
=======
list k = many1 k ||| valueParser []
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 8
-- Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if
--   * The input is empty
-- ~~~ Use bindParser, list and value. ~~~
many1 :: Parser a -> Parser [a]
<<<<<<< HEAD:src/L03/Parser.hs
many1 p = bindParser p (\a ->
          bindParser (list p) (\as ->
          valueParser (a:as)))
=======
many1 k = bindParser k (\k' ->
          bindParser (list k) (\kk' ->
          valueParser (k' : kk')))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 9
-- Return a parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
-- ~~~ The bindParser and character functions will be helpful here. ~~~
satisfy :: (Char -> Bool) -> Parser Char
<<<<<<< HEAD:src/L03/Parser.hs
satisfy p = P $ \input -> case input of
                            [] -> Error "eof"
                            (h:t) -> if p h then Value (t, h) else Error "Failed to satisfy"
=======
satisfy p = bindParser character (\c ->
            if p c then valueParser c else failed ("Unexpected character " ++ [c]))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 10.1
-- Return a parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
-- ~~~ Use the satisfy function. ~~~
is :: Char -> Parser Char
is c = satisfy (== c)

-- Exercise 10.2
-- Return a parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
-- ~~~ Use the satisfy and Data.Char.isDigit functions. ~~~
digit :: Parser Char
digit = satisfy isDigit

-- Exercise 10.3
-- Return a parser that produces zero or a positive integer but fails if
--   * The input is empty.
--   * The input does not produce a value series of digits
-- ~~~ Use the bindParser, valueParser, list and digit functions. ~~~
natural :: Parser Int
<<<<<<< HEAD:src/L03/Parser.hs
natural = bindParser (list digit) (\k -> case reads k of [] -> failed "Failed to parse natural"
=======
natural = bindParser (list digit) (\k -> case reads k of []    -> failed "Failed to parse natural"
>>>>>>> origin/nick:src/L05/Parser.hs
                                                         ((h,_):_) -> valueParser h)

-- Exercise 10.4
-- Return a parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
-- ~~~ Use the satisfy and Data.Char.isSpace functions. ~~~
space :: Parser Char
space = satisfy isSpace

-- Exercise 10.5
-- Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
-- ~~~ Use the many1 and space functions. ~~~
spaces1 :: Parser String
spaces1 = many1 space

-- Exercise 10.6
-- Return a parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
-- ~~~ Use the satisfy and Data.Char.isLower functions. ~~~
lower :: Parser Char
lower = satisfy isLower

-- Exercise 10.7
-- Return a parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
-- ~~~ Use the satisfy and Data.Char.isUpper functions. ~~~
upper :: Parser Char
upper = satisfy isUpper

-- Exercise 10.8
-- Return a parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
-- ~~~ Use the satisfy and Data.Char.isAlpha functions. ~~~
alpha :: Parser Char
alpha = satisfy isAlpha

-- Exercise 11
-- Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
-- ~~~ Use bindParser and value. ~~~
-- ~~~ Optionally use Prelude.foldr. If not, an explicit recursive call. ~~~
sequenceParser :: [Parser a] -> Parser [a]
<<<<<<< HEAD:src/L03/Parser.hs
sequenceParser = foldr (\a b -> bindParser a (\a' ->
                                bindParser b (\b' ->
                                valueParser (a':b')))) (valueParser [])
=======
sequenceParser []    = valueParser []
sequenceParser (h:t) = bindParser h (\a ->
                       bindParser (sequenceParser t) (\as ->
                       valueParser (a : as)))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 12
-- Return a parser that produces the given number of values off the given parser.
-- This parser fails if
--   * The given parser fails in the attempt to produce the given number of values.
-- ~~~ Use sequenceParser and Prelude.replicate. ~~~
thisMany :: Int -> Parser a -> Parser [a]
<<<<<<< HEAD:src/L03/Parser.hs
thisMany n = sequence . replicate n
=======
thisMany n p = sequenceParser (replicate n p)
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 13
-- Write a parser for Person.age.
-- * Age: positive integer
-- ~~~ Equivalent to natural. ~~~
ageParser :: Parser Int
ageParser = natural

-- Exercise 14
-- Write a parser for Person.firstName.
-- * First Name: non-empty string that starts with a capital letter
-- ~~~ Use bindParser, value, upper, list and lower. ~~~
firstNameParser :: Parser String
<<<<<<< HEAD:src/L03/Parser.hs
firstNameParser = bindParser upper (\u ->
                  bindParser (list lower) (\l ->
                  valueParser (u:l)))
=======
firstNameParser = bindParser upper (\c ->
                  bindParser (list lower) (\cs ->
                  valueParser (c : cs)))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 15
-- Write a parser for Person.surname.
-- * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
-- ~~~ Use bindParser, value, upper, thisMany, lower and list. ~~~
surnameParser :: Parser String
<<<<<<< HEAD:src/L03/Parser.hs
surnameParser = bindParser upper (\u ->
                bindParser (thisMany 5 lower) (\l -> 
                bindParser (list lower) (\m ->
                valueParser (u:l++m))))
=======
surnameParser = bindParser upper (\c ->
                bindParser (thisMany 5 lower) (\cs ->
                bindParser (list lower) (\t ->
                valueParser (c : cs ++ t))))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 16
-- Write a parser for Person.gender.
-- * Gender: character that must be 'm' or 'f'
  -- ~~~ Use is and (|||). ~~~
genderParser :: Parser Char
genderParser = is 'm' ||| is 'f'

-- Exercise 17
-- Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
-- * Phone: string of digits, dots or hyphens ...
-- ~~~ Use list, digit, (|||) and is. ~~~
phoneBodyParser :: Parser String
<<<<<<< HEAD:src/L03/Parser.hs
phoneBodyParser = undefined
=======
phoneBodyParser = list (digit ||| is '.' ||| is '-')
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 18
-- Write a parser for Person.phone.
-- * Phone: ... but must start with a digit and end with a hash (#)
-- ~~~ Use bindParser, value, digit, phoneBodyParser and is. ~~~
phoneParser :: Parser String
phoneParser = bindParser digit (\d ->
<<<<<<< HEAD:src/L03/Parser.hs
              bindParser (list (digit ||| is '.' ||| is '-')) (\b ->
              bindParser (is '#') (\_ ->
              valueParser (d:b)
              )))
=======
              bindParser phoneBodyParser (\z ->
              bindParser (is '#') (\_ ->
              valueParser (d : z))))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 19
-- Write a parser for Person.
-- ~~~ Use bindParser, value, (>>>)
--         ageParser,
--         firstNameParser,
--         surnameParser,
--         genderParser,
--         phoneParser ~~~
personParser :: Parser Person
personParser = bindParser ageParser (\a ->
<<<<<<< HEAD:src/L03/Parser.hs
               bindParser spaces1 (\_ ->
               bindParser firstNameParser (\f ->
               bindParser spaces1 (\_ -> 
               bindParser surnameParser (\s -> 
               bindParser spaces1 (\_ ->
               bindParser genderParser (\g ->
               bindParser spaces1 (\_ ->
               bindParser phoneParser (\p -> 
               valueParser (Person a f s g p))))))))))

personParser2 :: Parser Person
personParser2 = 
  do a <- ageParser
     spaces1
     f <- firstNameParser
     spaces1
     s <- surnameParser
     spaces1
     g <- genderParser
     spaces1
     p <- phoneParser
     return (Person a f s g p)
=======
               spaces1 >>>
               bindParser firstNameParser (\f ->
               spaces1 >>>
               bindParser surnameParser (\s ->
               spaces1 >>>
               bindParser genderParser (\g ->
               spaces1 >>>
               bindParser phoneParser (\p ->
               valueParser (Person a f s g p))))))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 20
-- Make sure all the tests pass!


-- Exercise 20.1
-- Write a Functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Functor Parser where
<<<<<<< HEAD:src/L03/Parser.hs
  fmap f a =
    bindParser a (valueParser . f)
=======
  fmap f x =
    bindParser x (valueParser . f)
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 20.2
-- Write an Applicative functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Applicative Parser where
  pure =
    valueParser
<<<<<<< HEAD:src/L03/Parser.hs
  f <*> a =
    bindParser f (\f' ->
    bindParser a (\a' ->
    valueParser (f' a')))
=======
  p <*> q =
    bindParser p (\f -> bindParser q (\a -> valueParser (f a)))
>>>>>>> origin/nick:src/L05/Parser.hs

-- Exercise 20.3
-- Write a Monad instance for a Parser.
instance Monad Parser where
  return =
    valueParser
  (>>=) =
    bindParser
