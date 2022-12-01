#!/usr/bin/env bash

solutions="solutions/Days"

cd "$(git rev-parse --show-toplevel)"

mkdir "$solutions"

for i in $(printf "%02i " {1..24})
do
  cat << EOF > "$solutions/Day$i.hs"
module Days.Day$i (day$i) where

import AOC (Solution (..))
import qualified Data.Text as T

day$i :: Solution
day$i = Solution parseInput part1 part2

parseInput :: T.Text -> a
parseInput = error "parseInput not defined for day $i"

part1 :: a -> Int
part1 = error "part1 not defined for day $i"

part2 :: a -> Int
part2 = error "part2 not defined for day $i"
EOF
done
