module Utils.Parser (
  Parser,
  symbol,
  lexeme,
  name,
  decimal
) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
name :: Parser String
name = lexeme (many $ anySingleBut '\n')
decimal :: Num a => Parser a
decimal = lexeme L.decimal
