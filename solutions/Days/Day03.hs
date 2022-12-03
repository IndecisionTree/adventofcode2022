module Days.Day03 (day03) where

import AOC (Solution (..))
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.List (foldl1', intersect, nub)
import Data.List.Split (chunksOf)
import Data.Text qualified as T

day03 :: Solution
day03 = Solution parseInput part1 part2

parseInput :: T.Text -> [[Int]]
parseInput = fmap (fmap priority . T.unpack) . T.lines

part1 :: [[Int]] -> Int
part1 = sum . concatMap (intersections . halve)
  where
    halve xs = chunksOf (length xs `div` 2) xs

part2 :: [[Int]] -> Int
part2 = sum . concatMap intersections . chunksOf 3 

priority :: Char -> Int
priority ch
  | isAsciiLower ch = fromEnum ch - fromEnum 'a' + 1
  | isAsciiUpper ch = fromEnum ch - fromEnum 'A' + 27
  | otherwise = error $ show ch

intersections :: Eq a => [[a]] -> [a]
intersections = nub . foldl1' intersect
