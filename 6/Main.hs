{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow
import           Data.List

main :: IO ()
main =
  readFile "input.txt"
    >>= print
    .   (sum . map (length . nub . concat) &&& sum . map
          (length . foldr1 intersect)
        )
    .   foldr
          (\l -> \case
            []        -> [[l]]
            (a : acc) -> if l == "" then [] : a : acc else (l : a) : acc
          )
          []
    .   lines
