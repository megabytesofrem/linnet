{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- AST
import AST
import Control.Monad.Combinators.Expr
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

-- | Parse something between parentheses, brackets, or braces.
enclosed :: Char -> Char -> Parser a -> Parser a
enclosed open close = between (char open >> sc) (char close >> sc)

symbol :: String -> Parser String
symbol = L.symbol sc

-- Primitive parsers

identP :: Parser String
identP = lexeme ((:) <$> letterChar <*> many alphas')
  where
    -- Allow ' and _ in identifiers
    alphas' = letterChar <|> alphaNumChar <|> char '\'' <|> char '_'

-- Literals
integerP :: Parser Integer
integerP = lexeme L.decimal

floatP :: Parser Double
floatP = lexeme L.float

stringLitP :: Parser String
stringLitP = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

literalP :: Parser Literal
literalP =
  choice
    [ LitInt <$> integerP, -- 42
      LitFloat <$> floatP, -- 3.14
      LitString <$> stringLitP, -- "string literals"
      LitBool True <$ symbol "true",
      LitBool False <$ symbol "false"
    ]

-- Expression parser

-- Lists and tuples: [1, 2, 3], (1, 2, 3)
listP :: Parser Expr
listP = enclosed '[' ']' (EList <$> sepBy exprP (symbol ","))

tupleP :: Parser Expr
tupleP = enclosed '(' ')' (ETuple <$> sepBy exprP (symbol ","))

applicationP :: Parser Expr
applicationP = do
  f <- termP
  args <- many termP
  pure $ foldl EApp f args

lambdaP :: Parser Expr
lambdaP = do
  _ <- symbol "\\"
  binders <- some identP
  _ <- symbol "->"
  ELam binders <$> exprP

-- Term parser
termP :: Parser Expr
termP =
  choice
    [ parens exprP,
      listP,
      tupleP,
      lambdaP,
      ELit <$> literalP,
      EIdent <$> identP
    ]
  where
    parens = enclosed '(' ')'

-- Higher in the list = higher precedence.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" (EUnaryOp Negate)],
    -- Left-associative binary operators
    [binary "*" (EBinOp Mul) AssocLeft],
    [binary "/" (EBinOp Div) AssocLeft],
    [binary "+" (EBinOp Add) AssocLeft],
    [binary "-" (EBinOp Sub) AssocLeft],
    [binary "==" (EBinOp Eq) AssocNone],
    [binary "/=" (EBinOp Neq) AssocNone],
    [binary "<" (EBinOp Lt) AssocNone],
    [binary ">" (EBinOp Gt) AssocNone],
    [binary "<=" (EBinOp LtEq) AssocNone],
    [binary ">=" (EBinOp GtEq) AssocNone],
    -- Functional operators
    [binary "." (EBinOp Compose) AssocRight],
    [binary "$" (EBinOp Apply) AssocRight],
    [binary ">>=" (EBinOp Bind) AssocLeft],
    [binary ">>" (EBinOp Pipe) AssocLeft],
    [binary "<$>" (EBinOp Map) AssocLeft],
    [binary "<*>" (EBinOp UFO) AssocLeft],
    [binary "<|>" (EBinOp Alt) AssocLeft]
  ]
  where
    prefix sym f = Prefix (f <$ symbol sym)
    binary sym f assoc = case assoc of
      AssocLeft -> InfixL (f <$ symbol sym)
      AssocRight -> InfixR (f <$ symbol sym)
      AssocNone -> InfixN (f <$ symbol sym)

exprP :: Parser Expr
exprP = makeExprParser applicationP operatorTable