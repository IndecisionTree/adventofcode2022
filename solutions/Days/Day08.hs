module Days.Day08 (day08) where

import AOC (Solution(..))

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict
import Data.Array (Array)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Text (Text)

import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- TODO whole day needs a refactor and a performance check. Part 1
-- seems fine, but part 2 is entirely bruteforce. Still runs fast on
-- my machine, but there has to be a better way.

day08 :: Solution
day08 = Solution parseInput part1 part2

type Index = (Int, Int)
type Grid = Array Index Int

type VisibleMap = M.Map Index Bool

parseInput :: Text -> Grid
parseInput input = A.listArray is $ digitToInt <$> T.unpack (T.concat lns)
  where
    lns = T.lines input
    rows = length lns
    cols = T.length (head lns)
    is = ((0, 0), (cols-1, rows-1))

part1 :: Grid -> Int
part1 g = M.size . M.filter id . bottom . top . right . left $ M.empty
  where
    (_, (rows, cols)) = A.bounds g

    lefts = [[(i, j) | j <- [0..cols]] | i <- [0..rows]]
    rights = [[(i, j) | j <- [cols, cols-1..0]] | i <- [0..rows]]
    tops = [[(i, j) | i <- [0..rows]] | j <- [0..cols]]
    bottoms = [[(i, j) | i <- [rows, rows-1..0]] | j <- [0..cols]]

    left m = foldl' computeVisible m lefts
    right m = foldl' computeVisible m rights
    top m = foldl' computeVisible m tops
    bottom m = foldl' computeVisible m bottoms
    
    computeVisible :: VisibleMap -> [Index] -> VisibleMap
    computeVisible vm = flip evalState (-1) . foldM go vm
      where
        go :: VisibleMap -> Index -> State Int VisibleMap
        go m i = do
          mx <- get
  
          let v = g A.! i
              visible = M.findWithDefault False i m
      
          if v > mx
            then put v >> return (M.insert i True m)
            else return (M.insert i visible m)

-- TODO find a way to not bruteforce this
part2 :: Grid -> Int
part2 g = maximum $ scenic <$> A.indices g
  where
    (_, (rows, cols)) = A.bounds g

    scenic :: Index -> Int
    scenic x@(i, j) = lefts * rights * tops * bottoms
      where
        lefts = cnt + if cnt < j && j > 0 then 1 else 0
          where cnt = countScenic x [(i, j') | j' <- [j-1, j-2..0]]

        rights = cnt + if cnt < (cols-j) && j < cols then 1 else 0
          where cnt = countScenic x [(i, j') | j' <- [j+1..cols]]

        tops = cnt + if cnt < i && i > 0 then 1 else 0
          where cnt = countScenic x [(i', j) | i' <- [i-1, i-2..0]]

        bottoms = cnt + if cnt < (rows-i) && i < rows then 1 else 0
          where cnt = countScenic x [(i', j) | i' <- [i+1..rows]]

    countScenic :: Index -> [Index] -> Int
    countScenic ind = length . takeWhile ((< val) . (g A.!))
      where
        val = g A.! ind

