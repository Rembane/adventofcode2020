module Main where

import           Control.Arrow
import           Data.List
import           Debug.Trace

twinPicker :: ([Int], [Int]) -> (Int, Int)
twinPicker (x : xs, y : ys) =
  let s = x + y
  in  traceShow ((x, y), s) $ case compare s 2020 of
        LT -> twinPicker (x : xs, ys)
        EQ -> (x, y)
        GT -> twinPicker (xs, y : ys)

main :: IO ()
main =
  readFile "input.txt"
    >>= ( print
        . uncurry (*)
        . twinPicker
        . (reverse &&& id)
        . sort
        . map read
        . lines
        )
