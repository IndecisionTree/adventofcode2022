module Days.Day06 (day06) where

import AOC (Solution (..))
import qualified Data.Text as T
import Data.List (find, nub)
import Data.Maybe (fromJust)

day06 :: Solution
day06 = Solution parseInput part1 part2

parseInput :: T.Text -> String
parseInput = T.unpack

part1 :: String -> Int
part1 = findUniq 4

part2 :: String -> Int
part2 = findUniq 14

findUniq :: Eq a => Int -> [a] -> Int
findUniq n xs = (+n) $ fromJust $ find f [0..length xs - n]
  where
    f i = s == nub s
      where s = take n $ drop i xs
