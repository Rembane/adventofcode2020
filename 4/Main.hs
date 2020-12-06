{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow
import           Data.Char
import           Data.Ix
import           Data.List
import qualified Data.Set                      as S
import           Text.Read

requiredFields :: S.Set String
requiredFields = S.fromList $ words "byr iyr eyr hgt hcl ecl pid"

hairColours :: S.Set String
hairColours = S.fromList $ words "amb blu brn gry grn hzl oth"

validate :: (String, String) -> Bool
validate = \case
  ("byr", s) -> maybe False (inRange (1920, 2002)) (readMaybe s)
  ("iyr", s) -> maybe False (inRange (2010, 2020)) (readMaybe s)
  ("eyr", s) -> maybe False (inRange (2020, 2030)) (readMaybe s)
  ("hgt", s) ->
    let suffix = dropWhile isDigit s
        num    = read $ takeWhile isDigit s
    in  case suffix of
          "in" -> inRange (59, 76) num
          "cm" -> inRange (150, 193) num
          _    -> False
  ("hcl", '#' : s) -> length (filter isHexDigit s) == 6
  ("ecl", s      ) -> S.member s hairColours
  ("pid", s      ) -> length (filter isDigit s) == 9
  ("cid", _      ) -> True
  _                -> False

isValidPassport :: [String] -> Bool
isValidPassport =
  ( uncurry (&&)
    . (   (S.null . S.difference requiredFields . S.fromList . map fst)
      &&& all validate
      )
    )
    . concatMap (map (second tail . span (/= ':')) . words)

main :: IO ()
main =
  readFile "input.txt"
    >>= print
    .   length
    .   filter isValidPassport
    .   foldr
          (\l -> \case
            []        -> [[l]]
            (a : acc) -> if l == "" then [] : a : acc else (l : a) : acc
          )
          []
    .   lines
