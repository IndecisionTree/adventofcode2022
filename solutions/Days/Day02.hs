module Days.Day02 (day02) where

import AOC (Solution (..))
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

day02 :: Solution
day02 = Solution parseInput part1 part2

data RPS = Rock | Paper | Scissors
  deriving (Show, Eq, Bounded)

data Action = Lose | Draw | Win
  deriving (Show, Eq)

instance Enum RPS where
  fromEnum rps = case rps of
    Rock -> 1
    Paper -> 2
    Scissors -> 3
  toEnum n = case n of
    1 -> Rock
    2 -> Paper
    3 -> Scissors
    _ -> error $ "Cannot convert to RPS: " ++ show n

parseInput :: T.Text -> [(RPS, T.Text)]
parseInput = fmap pLine . T.lines
  where
    pLine l = let [a, b] = T.words l in (translate table a, b)
    table = [("A", Rock), ("B", Paper), ("C", Scissors)]

part1 :: [(RPS, T.Text)] -> Int
part1 = sum . fmap (score . second (translate table))
  where
    table = [("X", Rock), ("Y", Paper), ("Z", Scissors)]

part2 :: [(RPS, T.Text)] -> Int
part2 = sum . fmap (score . matchAction . second (translate table))
  where
    table = [("X", Lose), ("Y", Draw), ("Z", Win)]
    matchAction (rps, action) = (rps, match)
      where
        match = case action of
          Lose -> prevRPS rps
          Draw -> rps
          Win -> nextRPS rps

translate :: (Show a, Eq a) => [(a, b)] -> a -> b
translate table ch =
  fromMaybe (error $ "Lookup failed: " ++ show ch) $ lookup ch table

score :: (RPS, RPS) -> Int
score (a, b) = fromEnum b + beats a b

beats :: RPS -> RPS -> Int
beats a b
  | nextRPS a == b = 6
  | a == b = 3
  | otherwise = 0

nextRPS :: RPS -> RPS
nextRPS rps | rps == maxBound = minBound
            | otherwise = succ rps
  
prevRPS :: RPS -> RPS
prevRPS rps | rps == minBound = maxBound
            | otherwise = pred rps
