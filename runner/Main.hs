module Main where

import AOC (Solution (..), mkAocClient)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.Functor (void)
import Data.IntMap.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import Solutions (solutions)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import Tests (test)
import Text.Printf (printf)

-- | CLI Options
data Options = Option
  { _command :: Action,
    _day :: Int,
    _part :: Maybe Int
  }

-- | CLI Commands
data Action = Run | Test | Bench | Submit
  deriving (Show)

main :: IO ()
main = do
  void $ loadFile defaultConfig

  session <- getEnv "SESSION"
  year <- read <$> getEnv "YEAR"

  Option {..} <- customExecParser (prefs showHelpOnEmpty) opts

  when (_day < 1 || _day > 24) $ do
    fail $ printf "Day '%d' is out of range (1-24)" _day

  (aocInput, aocSubmit) <- mkAocClient session year _day

  input <- getPuzzleInput _day aocInput
  let day = solutions M.! _day

  case _command of
    Run -> putStr $ run day _part input
    Test -> test day _day _part
    a -> fail $ "Unimplemented: " ++ show a

-- | Retrieve puzzle input for a given day from a file. If no file is
-- found, hit the api.
getPuzzleInput :: Int -> IO T.Text -> IO T.Text
getPuzzleInput day aocInput =
  T.readFile fp `catch` \(_ :: IOException) -> fetchInput
  where
    fp = printf ".inputs/%d.txt" day

    fetchInput = do
      input <- aocInput
      createDirectoryIfMissing True ".inputs"
      T.writeFile fp input
      return input

-- | Run solution (optionally just part) on input and return the
-- result as a string ready for submission
run :: Solution -> Maybe Int -> T.Text -> String
run (Solution pInput part1 part2) part input =
  case part of
    Nothing -> printf "Part 1: %s\nPart 2: %s\n" part1' part2'
    Just n -> printf "Part %d: %s\n" n $ if n == 1 then part1' else part2'
  where
    parsed = pInput input
    part1' = show $ part1 parsed
    part2' = show $ part2 parsed

-- | CLI parser
opts :: ParserInfo Options
opts =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Run, benchmark, test, or submit an AOC day"
        <> header "runner - an AOC solution runner"
    )
  where
    commands = subparser $ runCmd <> testCmd <> benchCmd <> submitCmd
    runCmd =
      mkCmd
        "run"
        (Option Run <$> day <*> optional part)
        "Run a given day, optionally specifying which part"
    testCmd =
      mkCmd
        "test"
        (Option Test <$> day <*> optional part)
        "Test a given day, optionally specifying which part"
    benchCmd =
      mkCmd
        "bench"
        (Option Bench <$> day <*> optional part)
        "Benchmark a given day, optionally specifying which part"
    submitCmd =
      mkCmd
        "submit"
        (Option Submit <$> day <*> (Just <$> part))
        "Run and submit the answer to a given day and part"

    day = argument auto (metavar "DAY")
    part = argument auto (metavar "PART")

    mkCmd cmd f desc = command cmd (info f (progDesc desc))
