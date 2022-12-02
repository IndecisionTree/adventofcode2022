module Days.Day02 (day02) where

import AOC (Solution (..))
import Control.Arrow ((>>>), (***), second)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

day02 :: Solution
day02 = Solution parseInput part1 part2

data RPS = Rock | Paper | Scissors
  deriving (Show, Eq, Enum, Bounded)

data Action = Lose | Draw | Win
  deriving (Show, Eq)

parseInput :: T.Text -> [(RPS, T.Text)]
parseInput = fmap (T.breakOn " " >>> translate table *** T.tail) . T.lines
  where
    table = [("A", Rock), ("B", Paper), ("C", Scissors)]

part1 :: [(RPS, T.Text)] -> Int
part1 = sum . fmap (score . second (translate table))
  where
    table = [("X", Rock), ("Y", Paper), ("Z", Scissors)]

part2 :: [(RPS, T.Text)] -> Int
part2 = sum . fmap (score . matchAction . second (translate table))
  where
    table = [("X", Lose), ("Y", Draw), ("Z", Win)]

translate :: (Show a, Eq a) => [(a, b)] -> a -> b
translate table ch =
  fromMaybe (error $ "Lookup failed: " ++ show ch) $ lookup ch table

matchAction :: (RPS, Action) -> (RPS, RPS)
matchAction (rps, action) = (rps, match)
  where
    match = case action of
      Lose -> prevRPS rps
      Draw -> rps
      Win -> nextRPS rps

score :: (RPS, RPS) -> Int
score (a, b) = (fromEnum b + 1) + beats a b

beats :: RPS -> RPS -> Int
beats a b
  | nextRPS a == b = 6
  | a == b = 3
  | otherwise = 0

nextRPS, prevRPS :: RPS -> RPS
nextRPS rps
  | rps == maxBound = minBound
  | otherwise = succ rps
prevRPS rps
  | rps == minBound = maxBound
  | otherwise = pred rps
