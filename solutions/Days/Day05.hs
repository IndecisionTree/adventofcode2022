module Days.Day05 (day05) where

import AOC (Solution (..))
import Control.Arrow
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Text qualified as T

day05 :: Solution
day05 = Solution parseInput part1 part2

-- TODO clean up this entire day, it's gross 🤮. Regex would probably
-- work nice here as the input is regular.

type Instruction = (Int, Int, Int)

-- TODO this is a place where Text is annoying and 'Solution' is
-- cumbersome to work with.  just end up unpacking it. Consider using
-- Stringlike or something else.

-- regex would be great here
parseInput :: T.Text -> ([String], [Instruction])
parseInput = T.unpack >>> splitOn "\n\n" >>> head &&& last >>> pCrates *** pMoves
  where
    pCrates = fmap (concat . init) . transpose . fmap pLine . lines
    pLine = fmap pCrate . chunksOf 4
    pCrate xs = [ch | ch /= ' ']
      where ch = xs !! 1

    pMoves = fmap pMove . lines
    pMove xs = (read n, read from - 1, read to - 1)
      where [_, n, _, from, _, to] = splitOn " " xs

part1 :: ([String], [Instruction]) -> String
part1 = fmap head . move reverse 

part2 :: ([String], [Instruction]) -> String
part2 = fmap head . move id

-- | Given a move function and some stacks and instructions, perform
-- the instructions on stacks using the move function.
move :: (String -> String) -> ([String], [Instruction]) -> [String]
move moveF (stacks, ins) = foldl' f stacks ins
  where
    f xs (n, from, to) = xs''
      where
        fromS = xs !! from
        toS = xs !! to
        xs' = update from (drop n fromS) xs
        xs'' = update to (moveF (take n fromS) ++ toS) xs'

-- TODO consider using Array or Vector (potentially mutable?) instead
-- of this update nonsense

-- | Replace an element at position 'n' in a list
update :: Int -> a -> [a] -> [a]
update n x xs
  | n < length xs = uncurry (++) . second ((x:) . tail) $ splitAt n xs
  | otherwise = error "Index out of bounds"
