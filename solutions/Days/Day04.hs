module Days.Day04 (day04) where

import AOC (Solution (..))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, some)
import Text.Megaparsec.Char (char, newline, numberChar)

day04 :: Solution
day04 = Solution parseInput part1 part2

type Range = (Int, Int)

parseInput :: T.Text -> [(Range, Range)]
parseInput = either (error . errorBundlePretty) id . parse pInput "" 
  where
    pInput :: Parsec Void T.Text [(Range, Range)]
    pInput = some $ pLine <* newline
    pLine = (,) <$> pRange <* char ',' <*> pRange
    pRange = (,) <$> pNum <* char '-' <*> pNum
    pNum = read <$> some numberChar

part1 :: [(Range, Range)] -> Int
part1 = length . filter f 
  where
    f ((a, b), (c, d)) = (a >= c && b <= d) || (c >= a && d <= b)

part2 :: [(Range, Range)] -> Int
part2 = length . filter f
  where
    f ((a, b), (c, d)) = (c >= a && c <= b) || (a >= c && a <= d)

