{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Convention: Internal parsers are prefixed with p, and exported parsers are given
-- canonical names like parseExpr, parseDecl, etc.
module Linnet.Parser
  ( pLiteral
  , pType
  , parseExpr
  , parseDecl
  )
where

import Linnet.AST.Declarations
import Linnet.AST.Operators

import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
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
  [ "forall"
  , "data"
  , "class"
  , "impl"
  , "def"
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
enclosed open close = between (symbol [open] >> sc) (symbol [close] >> sc)

enclosedStr :: String -> String -> Parser a -> Parser a
enclosedStr open close = between (symbol open >> sc) (symbol close >> sc)

sepByTokenOrEOL :: Parser a -> String -> Parser [a]
sepByTokenOrEOL p sep = sepEndBy1 p (symbol sep <|> eol)

symbol :: String -> Parser String
symbol = L.symbol sc

----------------------------------------
-- Primitive parsers

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
  pars <- many pBaseTy
  pure $ case pars of
    [] -> base
    ps -> case base of
      TVar name -> TCons name ps
      _ -> error "Type constructor must be a type variable"

pBinder :: Parser Binder
pBinder = do
  name <- pIdent
  ty <- optional (symbol ":" >> pType)
  pure $ Binder name ty

----------------------------------------
-- Expression parser

-- Lists and tuples: [1, 2, 3], (1, 2, 3)
pList :: Parser Expr
pList = enclosed '[' ']' (EList <$> sepBy parseExpr (symbol ","))

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
  ELam binders <$> parseExpr

pLet :: Parser Expr
pLet = do
  _ <- symbol "let"
  name <- pIdent
  ty <- optional (symbol ":" >> pType)
  _ <- symbol "="
  val <- parseExpr
  _ <- symbol "in"
  ELet (Binder name ty) val <$> parseExpr

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- parseExpr
  _ <- symbol "then"
  thenBranch <- parseExpr
  _ <- symbol "else"
  EIf cond thenBranch <$> parseExpr

pLoop :: Parser Expr
pLoop = do
  _ <- symbol "loop"
  binders <- many parseBinder
  symbol "{" >> ELoop binders <$> parseExpr <* symbol "}"
 where
  defaultValue ty = case ty of
    Just TInt -> ELit (LitInt 0)
    Just TFloat -> ELit (LitFloat 0.0)
    Just TBool -> ELit (LitBool False)
    Just TString -> ELit (LitString "")
    _ -> EUnit -- Default to unit if no type or unknown type

  -- Parse a loop binder (i: ty? = val?)
  parseBinder = do
    name <- pIdent
    ty <- optional (symbol ":" >> pType)
    val <- optional (symbol "=" >> parseExpr)
    pure $ LoopBinder name ty (fromMaybe (defaultValue ty) val)

-- Parse a monadic binding: x <- m
pMonadBind :: Parser Expr
pMonadBind = do
  ident <- pIdent
  _ <- symbol "<-"
  EBind ident <$> parseExpr

pMonadLet :: Parser Expr
pMonadLet = do
  _ <- symbol "let!"
  ident <- pIdent
  ty <- optional (symbol ":" >> pType)
  _ <- symbol "<-"
  ELetM (Binder ident ty) <$> parseExpr

-- NOTE: Do notation for monads e.g do ... end
pMonadDo :: Parser Expr
pMonadDo = do
  exprs <- enclosedStr "do" "end" (sepByTokenOrEOL validExpr ";")
  pure $ EBlock exprs
 where
  -- A valid expression in a monadic block can be a let!, bind, or a regular expression
  validExpr = pMonadLet <|> pMonadBind <|> parseExpr

-- Term parser
pTerm :: Parser Expr
pTerm =
  choice
    [ -- Parse monadic blocks first to allow let! and bind syntax inside them
      pMonadDo
    , pLet
    , pIf
    , pLoop
    , parenExprOrTuple
    , pList
    , pLambda
    , ELit <$> pLiteral
    , EIdent <$> pIdent
    ]
 where
  parenExprOrTuple = enclosed '(' ')' $ do
    exprs <- sepBy parseExpr (symbol ",")
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

parseExpr :: Parser Expr
parseExpr = makeExprParser pApp operatorTable

----------------------------------------
--  Declaration parsers

-- Parse a valid data constructor in the format of either:
--   - ConstructorName T T' ...
--   - ConstructorName (T, T', ...)
--   - ConstructorName { field1: T, field2: T', ... }

pDataTupleConstructor :: Parser (String, [Ty])
pDataTupleConstructor = do
  name <- pIdent
  tys <- enclosed '(' ')' (sepBy pType (symbol ","))
  pure (name, tys)

pDataTypedConstructor :: Parser (String, [Ty])
pDataTypedConstructor = do
  name <- pIdent
  tys <- some pType
  pure (name, tys)

pDataConstructor :: Parser (String, [Ty])
pDataConstructor = try pDataTupleConstructor <|> pDataTypedConstructor

pDataDeclaration :: Parser Decl
pDataDeclaration = do
  _ <- symbol "data"
  typeName <- pIdent
  typeParams' <- many pIdent
  constructors <- many pDataConstructor
  pure $ DataDeclaration typeName typeParams' constructors

pFunctionSignature :: Parser (String, [Binder], Maybe Ty)
pFunctionSignature = do
  -- name : a -> b -> c
  name <- pIdent
  _ <- symbol ":"
  ty <- pType
  pure (name, [], Just ty) -- Fill this in when parsing the full declaration

pFunctionDeclaration :: Parser Decl
pFunctionDeclaration = do
  -- name : a -> b -> c
  -- def name x y = expr
  (name, _params, mty) <- pFunctionSignature
  _ <- symbol "def"
  _ <- symbol name -- Ensure the function name is repeated in the definition
  paramBinders <- many pBinder
  _ <- symbol "="
  body <- parseExpr

  -- Convert parameters into a lambda
  let paramNames = map (\(Binder n _) -> n) paramBinders
      lambdaBody = foldr (\pname acc -> ELam [pname] acc) body paramNames
  pure $
    FunctionDeclaration
      (FunctionDecl name paramBinders mty lambdaBody)
pClassDeclaration :: Parser Decl
pClassDeclaration = do
  -- class ClassName a b where
  --   method1 : a -> b
  --   method2 : a -> a

  _ <- symbol "class"
  className' <- pIdent
  typeParams' <- many pIdent
  _ <- symbol "where"
  methods' <- many parseMethod
  pure $ ClassDeclaration (TypeclassDecl className' typeParams' methods')
 where
  parseMethod = do
    methodName <- pIdent
    _ <- symbol ":"
    methodType <- pType
    pure (methodName, [], methodType) -- No method type parameters for now

pClassImplementation :: Parser Decl
pClassImplementation = do
  -- impl ClassName MyType where
  --   def method1 a b = ...
  --   def method2 a b = ...

  _ <- symbol "impl"
  className' <- pIdent
  _ <- symbol ":"
  ty <- pType
  _ <- symbol "where"
  methods' <- many parseMethodImpl
  pure $ ClassImplementation (TypeclassImpl className' ty methods')
 where
  parseMethodImpl = do
    _ <- symbol "def"
    methodName <- pIdent
    methodExpr <- parseExpr
    pure (methodName, methodExpr)

parseDecl :: Parser Decl
parseDecl =
  choice
    [ try pFunctionDeclaration
    , try pDataDeclaration
    , try pClassDeclaration
    , try pClassImplementation
    , -- Fallback to parsing an expression declaration
      ExprDeclaration <$> parseExpr
    ]