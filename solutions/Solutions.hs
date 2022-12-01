module Solutions (solutions) where

import qualified Data.IntMap.Strict as M
import AOC (Solution)

import Days.Day01 (day01)
import Days.Day02 (day02)
import Days.Day03 (day03)
import Days.Day04 (day04)
import Days.Day05 (day05)
import Days.Day06 (day06)
import Days.Day07 (day07)
import Days.Day08 (day08)
import Days.Day09 (day09)
import Days.Day10 (day10)
import Days.Day11 (day11)
import Days.Day12 (day12)
import Days.Day13 (day13)
import Days.Day14 (day14)
import Days.Day15 (day15)
import Days.Day16 (day16)
import Days.Day17 (day17)
import Days.Day18 (day18)
import Days.Day19 (day19)
import Days.Day20 (day20)
import Days.Day21 (day21)
import Days.Day22 (day22)
import Days.Day23 (day23)
import Days.Day24 (day24)

solutions :: M.IntMap Solution
solutions = M.fromList [
  (1, day01),
  (2, day02),
  (3, day03),
  (4, day04),
  (5, day05),
  (6, day06),
  (7, day07),
  (8, day08),
  (9, day09),
  (10, day10),
  (11, day11),
  (12, day12),
  (13, day13),
  (14, day14),
  (15, day15),
  (16, day16),
  (17, day17),
  (18, day18),
  (19, day19),
  (20, day20),
  (21, day21),
  (22, day22),
  (23, day23),
  (24, day24) ]
