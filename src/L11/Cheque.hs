module L11.Cheque where

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

data Numbers =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord, Show, Enum)

data Teens =
  Eleven
  | Twelve
  | Thirteen
  | Fourteen
  | Fifthteen
  | Sixteen
  | Seventeen
  | Eighteen
  | Nineteen
  deriving (Eq, Ord, Show, Enum)

data Ty =
  Twenty
  | Thirty
  | Forty
  | Fifty
  | Sixty
  | Seventy
  | Eighty
  | Ninety
  deriving (Eq, Ord, Show, Enum)


fromChar ::
  Char
  -> Maybe Numbers
fromChar '0' =
  Just Zero
fromChar '1' =
  Just One
fromChar '2' =
  Just Two
fromChar '3' =
  Just Three
fromChar '4' =
  Just Four
fromChar '5' =
  Just Five
fromChar '6' =
  Just Six
fromChar '7' =
  Just Seven
fromChar '8' =
  Just Eight
fromChar '9' =
  Just Nine
fromChar _ =
  Nothing

fromTyNumber ::
  Numbers
  -> Ty
fromTyNumber Two =
  Twenty
fromTyNumber Three =
  Thirty
fromTyNumber Four =
  Forty
fromTyNumber Five =
  Fifty
fromTyNumber Six =
  Sixty
fromTyNumber Seven =
  Seventy
fromTyNumber Eight =
  Eighty
fromTyNumber Nine =
  Ninety

toTeen ::
  Numbers
  -> Teens
toTeen One =
  Eleven
toTeen Two =
  Twelve
toTeen Three =
  Thirteen
toTeen Four =
  Fourteen
toTeen Five =
  Fifthteen
toTeen Six =
  Sixteen
toTeen Seven =
  Seventeen
toTeen Eight =
  Eighteen
toTeen Nine =
  Nineteen

toDot ::
  String
  -> ([Numbers], String)
toDot =
  let toDot' x [] =
        (x, [])
      toDot' x (h:t) =
        case fromChar h of
          Just n -> toDot' (n:x) t
          Nothing -> (if h == '.'
                        then
                          (,)
                        else
                           toDot') x t
  in toDot' []

cheque ::
  String
  -> String
cheque s =
  let (l,r) = toDot s
  in case (l,r) of
    l -> undefined--split3 l
    r -> undefined

data V12 a =
  V0
  | V1 a
  | V2 a a
  deriving (Eq, Show)

split3 ::
  [a]
  -> (V12 a, [(a, a, a)])
split3 [] = (V0, [])
split3 [a] = (V1 a, [])
split3 [a, b] = (V2 b a, [])
split3 (a:b:c:rest) = let (z,zz) = split3 rest in
                      (z, (c,b,a) : zz )

trippleE ::
  [(Numbers,Numbers,Numbers)]
  -> String
trippleE [] = ""
trippleE [(a,b,c)] = show a ++ " hundred" ++ ( fromMaybe' ( fmap (" and " ++) (tyOut (b,c)) ))

v12E ::
  V12 Char
  -> String
v12E V0 = ""
v12E (V1 a) = fromMaybe' (fmap show (fromChar a))
v12E (V2 a b) = fromMaybe' (fmap (\a' -> fromMaybe' (tyOut (a', fromMaybe Zero (fromChar b)))) (fromChar a))

fromMaybe' ::
  Maybe[Char]
  -> [Char]
fromMaybe' (Just a) = a
fromMaybe' Nothing = ""


tyOut ::
  (Numbers, Numbers)
  -> Maybe String
tyOut (Zero,Zero) = Nothing
tyOut (Zero,b) = Just (show b)
tyOut (a,Zero) = Just (show . fromTyNumber $ a)
tyOut (One,b) = Just (show (toTeen b))
tyOut (One,Zero) = Just "Ten"
tyOut (a,b) = let z = fromTyNumber a
              in  Just (show z ++ " " ++ show b)

r ::
  [(String, String)]
r = [("0", "zero dollars and zero cents"),
     ("1", "one dollar and zero cents"),
     ("1.", "one dollar and zero cents"),
     ("0.", "zero dollars and zero cents"),
     ("0.0", "zero dollars and zero cents"),
     ("1.0", "one dollar and zero cents"),
     ("a1a", "one dollar and zero cents"),
     ("a1a.a0.7b", "one dollar and seven cents"),
     ("100", "one hundred dollars and zero cents"),
     ("100.45", "one hundred dollars and forty-five cents"),
     ("100.07", "one hundred dollars and seven cents"),
     ("9abc9def9ghi.jkl9mno", "nine hundred and ninety-nine dollars and ninety cents"),
     ("12345.67", "twelve thousand three hundred and forty-five dollars and sixty-seven cents"),
     ("456789123456789012345678901234567890123456789012345678901234567890.12", "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents")
    ]


illion ::
  [String]
illion =
  let preillion ::
        [String -> String]
      preillion =
        [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        [String]
      postillion =
        [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in [
       "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ liftM2 ((=<<) (++)) preillion postillion
