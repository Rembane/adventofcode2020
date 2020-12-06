{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.List

countYes :: [String] -> Int
countYes = length . nub . concat

main :: IO ()
main =
  readFile "input.txt"
    >>= print
    .   sum
    .   map countYes
    .   foldr
          (\l -> \case
            []        -> [[l]]
            (a : acc) -> if l == "" then [] : a : acc else (l : a) : acc
          )
          []
    .   lines
