module Linnet.Parse.Lexer
  ( -- * Parser types
    Parser
  , ParserS

    -- * Lexer utilities
  , sc
  , lexeme
  , symbol
  , enclosed
  , enclosedStr
  , sepByTokenOrEOL
  , altSym

    -- * Primitive parsers
  , pIdent
  , pCtorName
  , pModuleName
  , pQualifiedName
  , pInteger
  , pFloat
  , pString
  , pLiteral

    -- * Reserved words
  , reserved
  , primitiveTypes
  , isReserved
  , isPrimitive
  ) where

import Control.Monad.State (StateT)
import Data.Void (Void)

import Data.List (intercalate)
import Linnet.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Parser type alias for convenience
type Parser = Parsec Void String

-- Wrap the parser is StateT to pass the fixity environment during parsing
type ParserS = StateT FixityEnv Parser

-- Common parsers

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "/-" "-/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed open close = between (symbol [open] >> sc) (symbol [close] >> sc)

enclosedStr :: String -> String -> Parser a -> Parser a
enclosedStr open close = between (symbol open >> sc) (symbol close >> sc)

sepByTokenOrEOL :: Parser a -> String -> Parser [a]
sepByTokenOrEOL p sep = p `sepEndBy1` (symbol sep <|> eol)

altSym :: String -> String -> Parser String
altSym s z = symbol s <|> symbol z

---------------------------------

reserved :: [String]
reserved =
  [ "infixl"
  , "infixr"
  , "infix"
  , "∀"
  , "forall"
  , "data"
  , "class"
  , "impl"
  , "def"
  , "if"
  , "then"
  , "else"
  , "for"
  , "match"
  , "with"
  , "let"
  , "let!"
  , "in"
  , "do"
  , "end"
  ]

primitiveTypes :: [String]
primitiveTypes = ["Int", "Float", "Bool", "String", "()"]

isReserved :: String -> Bool
isReserved name = name `elem` reserved || name `elem` primitiveTypes

isPrimitive :: String -> Bool
isPrimitive name = name `elem` primitiveTypes

-- * Primitive parsers
pIdentRaw :: Parser String
pIdentRaw = lexeme . try $ do
  name <- (:) <$> letterChar <*> many alphas'
  if isReserved name
    then fail $ "Identifier cannot be a reserved word: " ++ name
    else pure name
 where
  alphas' = letterChar <|> alphaNumChar <|> char '\'' <|> char '_'

-- Raw version of pCtorName that doesnt have any lexeme wrapping
pCtorRaw :: Parser String
pCtorRaw = do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '\'' <|> char '_')
  let name = c : cs
  if isReserved name
    then fail $ "Constructor cannot be a reserved word: " ++ name
    else pure name

-- Name parsing

pIdent :: Parser String
pIdent = try pQualifiedName <|> pIdentRaw

-- Identifiers for data constructors must start with an uppercase letter
pCtorName :: Parser String
pCtorName = lexeme . try $ pCtorRaw

-- Parse a dotted module name e.g Data.List or Control.Monad.Identity
pModuleName :: Parser String
pModuleName = intercalate "." <$> sepBy1 pCtorRaw (char '.')

-- Parse a qualified name e.g Data.Functor.map
pQualifiedName :: Parser String
pQualifiedName = lexeme . try $ do
  segments <- some (try $ pCtorRaw <* char '.')
  name <- pIdentRaw'
  pure $ intercalate "." segments ++ "." ++ name
 where
  -- raw ident with no lexeme wrapping
  pIdentRaw' = do
    name <- (:) <$> letterChar <*> many (letterChar <|> alphaNumChar <|> char '\'' <|> char '_')
    if isReserved name
      then fail $ "Identifier cannot be a reserved word: " ++ name
      else pure name

pInteger :: Parser Integer
pInteger = lexeme L.decimal

pFloat :: Parser Double
pFloat = lexeme L.float

pString :: Parser String
pString = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pLiteral :: Parser Literal
pLiteral =
  choice
    [ LitInt <$> pInteger -- 42
    , LitFloat <$> pFloat -- 3.14
    , LitString <$> pString -- "string literals"
    , LitBool True <$ symbol "True" -- True
    , LitBool False <$ symbol "False" -- False
    ]