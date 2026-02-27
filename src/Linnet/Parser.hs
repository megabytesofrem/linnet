{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Linnet.Parser
  ( pLiteral
  , pType
  , pExpr
  )
where

import Linnet.AST

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

reserved :: [String]
reserved =
  [ "data"
  , "class"
  , "impl"
  , "fn"
  , "if"
  , "then"
  , "else"
  , "for"
  , "loop"
  , "match"
  , "let"
  , "in"
  ]

isReserved :: String -> Bool
isReserved k = k `elem` reserved

-- | Parse something between parentheses, brackets, or braces.
enclosed :: Char -> Char -> Parser a -> Parser a
enclosed open close = between (char open >> sc) (char close >> sc)

symbol :: String -> Parser String
symbol = L.symbol sc

-- * Primitive parsers

pIdent :: Parser String
pIdent = lexeme $ try $ do
  name <- (:) <$> letterChar <*> many alphas'
  if isReserved name
    then fail $ "Reserved keyword " ++ show name ++ " cannot be an identifier"
    else pure name
 where
  -- Allow ' and _ in identifiers
  alphas' = letterChar <|> alphaNumChar <|> char '\'' <|> char '_'

-- Literals
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
    , LitBool True <$ symbol "true"
    , LitBool False <$ symbol "false"
    ]

-- * Type parsers

pBaseTy :: Parser Ty
pBaseTy =
  choice
    [ TInt <$ symbol "Int"
    , TFloat <$ symbol "Float"
    , TBool <$ symbol "Bool"
    , TString <$ symbol "String"
    , TUnit <$ symbol "()"
    , TVar <$> pIdent
    , parens pArrowTy
    ]
 where
  parens = enclosed '(' ')'

-- Parse arrow types: Int -> Int, (a -> b)
pArrowTy :: Parser Ty
pArrowTy = do
  base <- pBaseTy
  rest <- optional (symbol "->" >> pArrowTy)
  pure $ case rest of
    Just f -> TFn base f
    Nothing -> base

-- Parse types with type constructors
pType :: Parser Ty
pType = do
  base <- pArrowTy
  pars <- optional $ enclosed '[' ']' (sepBy pType (symbol ","))
  pure $ case pars of
    Just ps -> case base of
      TVar name -> TCons name ps
      _ -> error "Type constructor must be a type variable"
    Nothing -> base

-- * Expression parser

-- Lists and tuples: [1, 2, 3], (1, 2, 3)
pList :: Parser Expr
pList = enclosed '[' ']' (EList <$> sepBy pExpr (symbol ","))

pApp :: Parser Expr
pApp = do
  f <- pTerm
  args <- many pTerm
  pure $ foldl EApp f args

pLambda :: Parser Expr
pLambda = do
  _ <- symbol "\\"
  binders <- some pIdent
  _ <- symbol "->"
  ELam binders <$> pExpr

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- pExpr
  _ <- symbol "then"
  thenBranch <- pExpr
  _ <- symbol "else"
  EIf cond thenBranch <$> pExpr

-- Term parser
pTerm :: Parser Expr
pTerm =
  choice
    [ pIf
    , parenExprOrTuple
    , pList
    , pLambda
    , ELit <$> pLiteral
    , EIdent <$> pIdent
    ]
 where
  parenExprOrTuple = enclosed '(' ')' $ do
    exprs <- sepBy pExpr (symbol ",")
    pure $ case exprs of
      [] -> EUnit -- () is the unit value
      [e] -> e -- Just a parenthesized expression
      es -> ETuple es -- A tuple

-- Higher in the list = higher precedence.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" (EUnaryOp Negate)]
  , -- Left-associative binary operators
    [binary "*" (EBinOp Mul) AssocLeft]
  , [binary "/" (EBinOp Div) AssocLeft]
  , [binary "+" (EBinOp Add) AssocLeft]
  , [binary "-" (EBinOp Sub) AssocLeft]
  , [binary "==" (EBinOp Eq) AssocNone]
  , [binary "/=" (EBinOp Neq) AssocNone]
  , [binary "<" (EBinOp Lt) AssocNone]
  , [binary ">" (EBinOp Gt) AssocNone]
  , [binary "<=" (EBinOp LtEq) AssocNone]
  , [binary ">=" (EBinOp GtEq) AssocNone]
  , -- Functional operators
    [binary "." (EBinOp Compose) AssocRight]
  , [binary "$" (EBinOp Apply) AssocRight]
  , [binary ">>=" (EBinOp Bind) AssocLeft]
  , [binary ">>" (EBinOp Pipe) AssocLeft]
  , [binary "<$>" (EBinOp Map) AssocLeft]
  , [binary "<*>" (EBinOp UFO) AssocLeft]
  , [binary "<|>" (EBinOp Alt) AssocLeft]
  ]
 where
  prefix sym f = Prefix (f <$ symbol sym)
  binary sym f assoc = case assoc of
    AssocLeft -> InfixL (f <$ symbol sym)
    AssocRight -> InfixR (f <$ symbol sym)
    AssocNone -> InfixN (f <$ symbol sym)

pExpr :: Parser Expr
pExpr = makeExprParser pApp operatorTable