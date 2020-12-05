module Main where

import           Control.Arrow
import           Data.Maybe
import qualified Data.Set                      as S

newtype Pos = Pos { unPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

parseMap :: String -> S.Set Pos
parseMap =
  S.fromList
    . concat
    . zipWith
        (\y r ->
          mapMaybe (\(x, c) -> if c == '#' then Just $ Pos (x, y) else Nothing)
            $ zip [0 ..] r
        )
        [0 ..]
    . lines

modWidthPos :: Pos -> Pos
modWidthPos = Pos . first (`mod` 31) . unPos

parametricSolution :: (Pos -> Pos) -> Pos -> S.Set Pos -> Int
parametricSolution f p s
  | snd (unPos p) > 322
  = 0
  | otherwise
  = (if S.member p s then 1 else 0) + parametricSolution f (modWidthPos (f p)) s

solve1 :: S.Set Pos -> Int
solve1 = parametricSolution (Pos . ((+ 3) *** (+ 1)) . unPos) (Pos (0, 0))

solve2 :: S.Set Pos -> Int
solve2 s = product $ map
  ( (\f -> parametricSolution (Pos . f . unPos) (Pos (0, 0)) s)
  . (\(x, y) -> (x +) *** (y +))
  )
  [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

-- | (0,0) is topleft corner. It's simpler that way.
main :: IO ()
main = readFile "input.txt" >>= print . solve2 . parseMap
