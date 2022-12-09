module Days.Day07 (day07, parseInput) where

import AOC (Solution(..))
import Utils.Tree (BTree(..), Context(..), Zipper)

import Control.Monad.Trans.State.Strict (State, execState, evalState, modify)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Utils.Tree as Z

day07 :: Solution
day07 = Solution parseInput part1 part2

type FileSystem = BTree FSType

data FSType = Dir Text | File Text Int
  deriving (Eq, Show)

parseInput :: Text -> FileSystem
parseInput = Z.unZipper . foldl' createFS (Node (Dir "/") [], Root) . drop 1 . T.lines

createFS :: Zipper FSType -> Text -> Zipper FSType
createFS z t = case T.words t of
  ["$", "cd", ".."] -> Z.up z
  ["$", "cd", dir] -> Z.down (fromJust $ Z.findChild (findDir dir) z) z
  ["$", "ls"] -> z
  ["dir", dir] -> Z.insert (Node (Dir dir) []) z
  [size, file] -> Z.insert (Leaf $ File file (read $ T.unpack size)) z
  _ -> error $ "Unrecognized command: " ++ show t
  where
    findDir :: Text -> FSType -> Bool
    findDir _ (File _ _) = False
    findDir dir (Dir name) = name == dir

part1 :: FileSystem -> Int
part1 = flip execState 0 . sumTree lessThan
  where
    lessThan size = if size <= 100000 then (+size) else id

part2 :: FileSystem -> Int
part2 fs = flip execState total $ sumTree minSpace fs
  where
    total = flip evalState 0 $ sumTree (const id) fs
    minNeeded = total - 40000000
    minSpace size = if size >= minNeeded then min size else id

sumTree :: (Int -> Int -> Int) -> FileSystem -> State Int Int
sumTree _ (Leaf (Dir _)) = error "Impossible tree: Dir in Leaf"
sumTree _ (Leaf (File _ size)) = return size
sumTree f (Node _ children) = do
  sizes <- mapM (sumTree f) children
  let size = sum sizes
  modify (f size)
  return size
