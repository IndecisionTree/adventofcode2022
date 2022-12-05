{-# LANGUAGE OverloadedStrings #-}

module Tests (test) where

import AOC (Solution (..), showSolution)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Ingredients (tryIngredients)
import Test.Tasty.Ingredients.Basic (consoleTestReporter)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer qualified as L
import Type.Reflection

data TestInput = TestInput
  { _testName :: T.Text,
    _testInput :: T.Text,
    _testExpected :: T.Text
  }
  deriving (Show)

type Parser = Parsec Void T.Text

-- | Test solution via tests in 'data/examples/(Day #)/part{1,2}.txt'
test :: Solution -> Int -> Maybe Int -> IO ()
test (Solution pInput part1 part2) day part = do
  p1 <- T.readFile $ "data" </> "examples" </> show day </> "part1" <.> "txt"
  p2 <- T.readFile $ "data" </> "examples" </> show day </> "part2" <.> "txt"

  let p1Tests =
        testGroup "Part 1"
          . fmap (mkTest pInput part1)
          . either (error . errorBundlePretty) id
          . parse pTests "part1.txt"
          $ p1
      p2Tests =
        testGroup "Part 2"
          . fmap (mkTest pInput part2)
          . either (error . errorBundlePretty) id
          . parse pTests "part2.txt"
          $ p2
      tests = testGroup ("Day " ++ show day) $ case part of
        Nothing -> [p1Tests, p2Tests]
        Just 1 -> [p1Tests]
        _ -> [p2Tests]

  case tryIngredients [consoleTestReporter] mempty tests of
    Nothing -> error $ "Error running tests for day " ++ show day
    Just act -> do
      res <- act
      putStrLn $ if res then "Success" else "Failure"

-- | Given an input parser, part1 or part2 function, and a test input,
-- generate an HUnit test.
-- TODO this feels like a leaky abstraction of Solution
mkTest :: (Typeable b, Show b) => (T.Text -> a) -> (a -> b) -> TestInput -> TestTree
mkTest pInput part TestInput {..} =
  testCase (T.unpack _testName) $ T.unpack _testExpected @=? result
  where
    result = showSolution . part $ pInput _testInput

-- | Parse test files into TestInput's
-- TODO more robust parsing and better, user-friendly custom errors
pTests :: Parser [TestInput]
pTests = many pTest <* eof
  where
    pTest = TestInput <$> pName <*> pInput <*> pExpected <?> "Test"
    -- TODO handle leading space in test input more elegantly 
    pName = T.pack <$> (symbol "-" *> some (anySingleBut '\n') <* newline) <?> "Test Name"
    pInput = T.pack <$> someTill anySingle (symbol "==") <?> "Input Lines"
    pExpected = T.pack <$> many (anySingleBut '\n') <* newline <?> "Expected Output"

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space
