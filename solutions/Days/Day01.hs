module Days.Day01 (day01) where

import AOC (Solution (..))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.List (sortBy)

day01 :: Solution
day01 = Solution parseInput part1 part2

parseInput :: T.Text -> [Int]
parseInput = fmap (sum . fmap readInt . T.lines) . T.splitOn "\n\n"
  where
    readInt = either error fst . T.decimal

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . sortBy (flip compare)
