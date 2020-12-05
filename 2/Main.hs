module Main where

import           Data.Char
import           Data.Ix
import           Data.List
import           Debug.Trace

isValid :: String -> Bool
isValid s =
  let (lower, '-' : s'                         ) = parseNumber "" s
      (upper, ' ' : char : ':' : ' ' : password) = parseNumber "" s'
  in  case find ((== char) . head) $ group $ sort password of
        Just cs ->
          let r = inRange (lower, upper) $ length cs
          in  if r then r else traceShow (r, length cs, s) r
        _ -> False
 where
  parseNumber :: String -> String -> (Int, String)
  parseNumber r (c : cs) | isDigit c = parseNumber (c : r) cs
                         | otherwise = (read r, c : cs)

main :: IO ()
main = readFile "input.txt" >>= (print . length . filter isValid . lines)
