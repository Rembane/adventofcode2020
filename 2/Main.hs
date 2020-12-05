module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Char
import           Data.Ix
import           Data.List

data PasswordPolicy = PP (Int, Int) Char String

mparse :: (String -> a) -> State String a
mparse f = state ((f *** dropWhile (== ' ')) . span (/= ' '))

mread :: Read a => State String a
mread = mparse read

parsePasswordPolicy :: String -> PasswordPolicy
parsePasswordPolicy = evalState
  (   PP
  <$> mparse ((read *** read . tail) . span (/= '-'))
  <*> mparse head
  <*> mparse id
  )

-- 1-3 a: abcde
isValid :: PasswordPolicy -> Bool
isValid (PP r c p) = inRange r (length (filter (== c) p))

main :: IO ()
main =
  readFile "input.txt"
    >>= (print . length . filter isValid . map parsePasswordPolicy . lines)
