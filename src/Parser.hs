{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- AST
import AST
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Parser type (Void is the custom error type)
type Parser = Parsec Void String

-- | Space consumer: skips whitespace and comments.
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Consume whitespace after a parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbolP :: String -> Parser String
symbolP = L.symbol sc

-- Raw parsers
identP :: Parser String
identP = lexeme ((:) <$> letterChar <*> many alphas')
  where
    -- Allow ' and _ in identifiers
    alphas' = letterChar <|> alphaNumChar <|> char '\'' <|> char '_'

integerP :: Parser Integer
integerP = lexeme L.decimal

floatP :: Parser Double
floatP = lexeme L.float

stringLitP :: Parser String
stringLitP = lexeme (char '"' >> manyTill L.charLiteral (char '"'))
