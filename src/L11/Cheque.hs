module L11.Cheque where

import Data.List
import Data.Int
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Instances

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


data Digits =
  D1 Numbers
  | D2 Numbers Numbers
  | D3 Numbers Numbers Numbers
  deriving (Eq)

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

cheque ::
  String
  -> String
cheque s =
  let (p,l,r) = toDot (dropWhile (`notElem` ('.':['1'..'9'])) s)
      l' = case l of
             [] -> "zero dollars"
             [One] -> "one dollar"
             _ -> ill l p ++ " dollars"
      r' = case mapMaybe (fromChar) r of
             [] -> "zero cents"
             [Zero, One] -> "one cent"
             (a:b:_) -> show (D2 a b) ++ " cents"
             (a:_) -> show (D2 a Zero) ++ " cents"
  in l' ++ " and " ++ r'

ill ::
  [Numbers]
  -> Bool
  -> String
ill xx st =
  let space "" =
        ""
      space x =
        ' ':x
      state a =
        if not (null a) && st then "," else ""
      todigits acc _ [] =
        acc
      todigits _ [] _ =
        error "ha ha ha"
      todigits acc (_:it) (Zero:Zero:Zero:t) =
        todigits acc it t
      todigits acc (ih:it) (a:b:c:t) =
        todigits ((show (D3 c b a) ++ space ih ++ state acc):acc) it t
      todigits acc (_:it) (Zero:Zero:t) =
        todigits acc it t
      todigits acc (ih:it) (a:b:t) =
        todigits ((show (D2 b a) ++ space ih ++ state acc):acc) it t
      todigits acc (_:it) (Zero:t) =
        todigits acc it t
      todigits acc (ih:_) (a:_) =
        (show (D1 a) ++ space ih ++ state acc):acc
  in unwords $ todigits [] illion xx


toDot ::
  String
  -> (Bool, [Numbers], String)
toDot s =
  let toDot' ::
        [Numbers]
        -> Int
        -> String
        -> (Int, [Numbers], String)
      toDot' x i [] =
        (i, x, [])
      toDot' x i (h:t) =
        case fromChar h of
          Just n -> toDot' (n:x) (i+1) t
          Nothing -> (if h == '.'
                        then
                          (i, x, t)
                        else
                           toDot' x i t)
      (i', m, s') = toDot' [] 0 s
  in (i' >= 5, m, s')


instance Show Digits where
  show d =
    let showd x = case show x of
                  (h:t) -> (toLower h):t
                  [] -> ""
        x .++. y = x ++ if y == Zero then [] else '-' : showd y
    in case d of
        D1 a -> showd a
        D2 Zero b -> showd b
        D2 One b -> case b of
                      Zero -> "ten"
                      One -> "eleven"
                      Two -> "twelve"
                      Three -> "thirteen"
                      Four -> "fourteen"
                      Five -> "fifteen"
                      Six -> "sixteen"
                      Seven -> "seventeen"
                      Eight -> "eighteen"
                      Nine -> "nineteen"
        D2 Two b -> "twenty" .++. b
        D2 Three b -> "thirty" .++. b
        D2 Four b -> "forty" .++. b
        D2 Five b -> "fifty" .++. b
        D2 Six b -> "sixty" .++. b
        D2 Seven b -> "seventy" .++. b
        D2 Eight b -> "eighty" .++. b
        D2 Nine b -> "ninety" .++. b
        D3 Zero Zero Zero -> ""
        D3 Zero b c -> show (D2 b c)
        D3 a Zero Zero -> showd a ++ " hundred"
        D3 a b c -> showd a ++ " hundred and " ++ show (D2 b c)



test ::
  IO ()
test =
  mapM_ (\(i, o) -> let r = cheque i
                    in when (o /= r) (mapM_ putStrLn [i, '\t':o, '\t':r]))
    [
      ("0", "zero dollars and zero cents")
    , ("1", "one dollar and zero cents")
    , ("0.1", "zero dollars and ten cents")
    , ("1.", "one dollar and zero cents")
    , ("0.", "zero dollars and zero cents")
    , ("0.0", "zero dollars and zero cents")
    , (".34", "zero dollars and thirty-four cents")
    , ("0.3456789", "zero dollars and thirty-four cents")
    , ("1.0", "one dollar and zero cents")
    , ("1.01", "one dollar and one cent")
    , ("a1a", "one dollar and zero cents")
    , ("a1a.a0.7b", "one dollar and seven cents")
    , ("100", "one hundred dollars and zero cents")
    , ("100.0", "one hundred dollars and zero cents")
    , ("100.00", "one hundred dollars and zero cents")
    , ("100.00000", "one hundred dollars and zero cents")
    , ("1000456.13", "one million, four hundred and fifty-six dollars and thirteen cents")
    , ("1001456.13", "one million, one thousand, four hundred and fifty-six dollars and thirteen cents")
    , ("16000000456.13", "sixteen billion, four hundred and fifty-six dollars and thirteen cents")
    , ("100.45", "one hundred dollars and forty-five cents")
    , ("100.07", "one hundred dollars and seven cents")
    , ("9abc9def9ghi.jkl9mno", "nine hundred and ninety-nine dollars and ninety cents")
    , ("12345.67", "twelve thousand, three hundred and forty-five dollars and sixty-seven cents")
    , ("456789123456789012345678901234567890123456789012345678901234567890.12", "four hundred and fifty-six vigintillion, seven hundred and eighty-nine novemdecillion, one hundred and twenty-three octodecillion, four hundred and fifty-six septendecillion, seven hundred and eighty-nine sexdecillion, twelve quindecillion, three hundred and forty-five quattuordecillion, six hundred and seventy-eight tredecillion, nine hundred and one duodecillion, two hundred and thirty-four undecillion, five hundred and sixty-seven decillion, eight hundred and ninety nonillion, one hundred and twenty-three octillion, four hundred and fifty-six septillion, seven hundred and eighty-nine sextillion, twelve quintillion, three hundred and forty-five quadrillion, six hundred and seventy-eight trillion, nine hundred and one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety dollars and twelve cents")
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
       ""
     , "thousand"
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
