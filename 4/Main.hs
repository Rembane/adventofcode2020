{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.List
import qualified Data.Set                      as S

requiredFields :: S.Set String
requiredFields = S.fromList $ words "byr iyr eyr hgt hcl ecl pid"

isValidPassport :: [String] -> Bool
isValidPassport = S.null . S.difference requiredFields . S.fromList . concatMap
  (map (takeWhile (/= ':')) . words)

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
