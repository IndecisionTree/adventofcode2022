module Days.Day03 (day03) where

import AOC (Solution (..))
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.List.Split (chunksOf)
import Data.IntMap.Strict qualified as M
import Data.Text qualified as T

type Counter = M.IntMap Int

day03 :: Solution
day03 = Solution parseInput part1 part2

parseInput :: T.Text -> [T.Text]
parseInput = T.lines

part1 :: [T.Text] -> Int
part1 = sum
  . fmap (fst
          . head
          . M.toList
          . uncurry M.intersection
          . join bimap mkCounter
          . (T.splitAt =<< ((`div` 2) . T.length)))

part2 :: [T.Text] -> Int
part2 = sum
  . fmap (fst . head . M.toList . foldr1 M.intersection)
  . chunksOf 3
  . fmap mkCounter

priority :: Char -> Int
priority ch
  | isAsciiLower ch = fromEnum ch - 96
  | isAsciiUpper ch = fromEnum ch - 38
  | otherwise = error $ show ch

mkCounter :: T.Text -> Counter
mkCounter =
  M.fromListWith (+) . flip zip [1,1..] . fmap priority . T.unpack
