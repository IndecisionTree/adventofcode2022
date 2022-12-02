module Days.Day02 (day02) where

import AOC (Solution (..))
import Control.Arrow ((<<<), (***), second)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

day02 :: Solution
day02 = Solution parseInput part1 part2

-- | an RPS move
data RPS = Rock | Paper | Scissors
  deriving (Show, Eq, Enum, Bounded)

-- | a game outcome
data Outcome = Lose | Draw | Win
  deriving (Show, Eq)

-- | Parse the input into tuples consisting of (RPS move, "XYZ" Char).
parseInput :: T.Text -> [(RPS, T.Text)]
parseInput = fmap (lookup' table *** T.tail <<< T.breakOn " ") . T.lines
  where
    table = [("A", Rock), ("B", Paper), ("C", Scissors)]

-- | Part 1, translate the "XYZ" chars into RPS moves, score the
-- results, sum them together.
part1 :: [(RPS, T.Text)] -> Int
part1 = sum . fmap (score . second (lookup' table))
  where
    table = [("X", Rock), ("Y", Paper), ("Z", Scissors)]

-- | Part 2, translate the "XYZ" chars into game outcome Actions,
-- match those actions with the necessary RPS move to achieve the
-- desired action, score the results, and sum them together.
part2 :: [(RPS, T.Text)] -> Int
part2 = sum . fmap (score . matchAction . second (lookup' table))
  where
    table = [("X", Lose), ("Y", Draw), ("Z", Win)]

-- | Wrapper around 'lookup' that 'error's out if the lookup fails.
lookup' :: (Show a, Eq a) => [(a, b)] -> a -> b
lookup' table a =
  fromMaybe (error $ "Lookup failed: " ++ show a) $ lookup a table

-- | Translate a desired 'Outcome' into an 'RPS' move based on the
-- 'RPS' move given for player 1 in the first tuple position.
matchAction :: (RPS, Outcome) -> (RPS, RPS)
matchAction (rps, action) = (rps, match)
  where
    match = case action of
      Lose -> prevRPS rps
      Draw -> rps
      Win -> nextRPS rps

-- | Score an RPS round. Uses the derived 'Enum' instance for 'RPS' to
-- determine a moves individual value.
score :: (RPS, RPS) -> Int
score (a, b) = (fromEnum b + 1) + beats a b

-- | Determine the points received by player 2 when facing player 1's
-- move.
beats :: RPS -> RPS -> Int
beats p1 p2
  | nextRPS p1 == p2 = 6
  | p1 == p2 = 3
  | otherwise = 0

-- | Bounded successor/predecessor for 'RPS' with wrap-around at both
-- upper and lower bounds. Uses 'RPS's derived 'Enum' and 'Bounded'
-- instances.
nextRPS, prevRPS :: RPS -> RPS
nextRPS rps
  | rps == maxBound = minBound
  | otherwise = succ rps
prevRPS rps
  | rps == minBound = maxBound
  | otherwise = pred rps
