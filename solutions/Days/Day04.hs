module Days.Day04 (day04) where

import AOC (Solution (..))
import Control.Arrow (Arrow (..), (>>>))
import qualified Data.Text as T
import qualified Data.Text.Read as T

day04 :: Solution
day04 = Solution parseInput part1 part2

type Range = (Int, Int)

parseInput :: T.Text -> [(Range, Range)]
parseInput = fmap pLine . T.lines
  where
    pLine = T.splitOn "," >>> fmap pRange >>> head &&& last
    pRange = T.splitOn "-" >>> fmap readInt >>> head &&& last
    readInt = either error fst . T.decimal

part1 :: [(Range, Range)] -> Int
part1 = length . filter f 
  where
    f ((s1, e1), (s2, e2)) = (s1 >= s2 && e1 <= e2) || (s2 >= s1 && e2 <= e1)

part2 :: [(Range, Range)] -> Int
part2 = length . filter f
  where
    f ((s1, e1), (s2, e2)) = s1 <= e2 && s2 <= e1
