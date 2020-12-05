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
isValid1 :: PasswordPolicy -> Bool
isValid1 (PP r c p) = inRange r (length (filter (== c) p))

isValid2 :: PasswordPolicy -> Bool
isValid2 (PP (p1, p2) c pass) =
  (== 1)
    . length
    . filter
        (uncurry (&&) . ((uncurry (||) . ((p1 ==) &&& (p2 ==))) *** (== c)))
    . zip [1 ..]
    $ pass

main :: IO ()
main =
  readFile "input.txt"
    >>= ( print
        . (length . filter isValid1 &&& length . filter isValid2)
        . map parsePasswordPolicy
        . lines
        )
