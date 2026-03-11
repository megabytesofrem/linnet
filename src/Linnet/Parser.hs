-- Convention: Internal parsers are prefixed with p, and exported parsers are given
-- canonical names like parseExpr, parseDecl, etc.
module Linnet.Parser
  ( pLiteral
  , pType
  , parseExpr
  , parseDecl
  )
where

import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Char (GeneralCategory (..), generalCategory)
import Data.Map.Strict qualified as M
import Linnet.AST.Declarations
import Linnet.AST.Operators
import Linnet.AST.Pattern (Pat (..))

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
  [ "infixl"
  , "infixr"
  , "infix"
  , "forall"
  , "∀"
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
  , "with"
  , "let"
  , "let!"
  , "in"
  , "do"
  , "end"
  , "true"
  , "false"
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

pCtorIdent :: Parser String
pCtorIdent = lexeme $ try $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let name = c : cs
  if isReserved name
    then fail $ "Reserved keyword " ++ show name ++ " cannot be a constructor"
    else pure name

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

pForAll :: Parser Ty
pForAll = do
  _ <- symbol "forall" <|> symbol "∀"
  typeVars <- some pIdent
  _ <- symbol "."
  ty <- pArrowTy

  -- TODO: Implement universial quantification later on in the type system
  pure undefined

pArrowTy :: Parser Ty
pArrowTy = do
  base <- pBaseTy
  rest <- optional (symbol "->" <|> symbol "→" >> pArrowTy)
  pure $ case rest of
    Just f -> TFn base f
    Nothing -> base

-- Parse types with type constructors
pType :: Parser Ty
pType = do
  -- TODO: Implement universal quantification later on in the type system
  quantify <- optional $ pForAll *> (symbol "=>" <|> symbol "⇒")

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
  _ <- symbol "\\" <|> symbol "λ"
  binders <- some pIdent
  _ <- symbol "->" <|> symbol "→"
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

pMatchBranch :: Parser (Pat, Expr)
pMatchBranch = do
  _ <- symbol "|"
  pat <- pPattern
  _ <- symbol "->" <|> symbol "→"
  (pat,) <$> parseExpr

pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  expr <- parseExpr
  _ <- symbol "with"
  branches <- some pMatchBranch
  pure $ EMatch expr branches

pLoop :: Parser Expr
pLoop = do
  _ <- symbol "loop"
  binders <- symbol "(" *> many parseBinder <* symbol ")"
  symbol "do" >> ELoop binders <$> parseExpr <* symbol "end"
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
  _ <- symbol "<-" <|> symbol "←"
  EBind ident <$> parseExpr

pMonadLet :: Parser Expr
pMonadLet = do
  _ <- symbol "let!"
  ident <- pIdent
  ty <- optional (symbol ":" >> pType)
  _ <- symbol "<-" <|> symbol "←"
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
    , pMatch
    , pLoop
    , parenExprOrTuple
    , pList
    , pLambda
    , ELit <$> pLiteral
    , EVar <$> pIdent
    ]
 where
  parenExprOrTuple = enclosed '(' ')' $ do
    exprs <- sepBy parseExpr (symbol ",")
    pure $ case exprs of
      [] -> EUnit -- () is the unit value
      [e] -> e -- Just a parenthesized expression
      es -> ETuple es -- A tuple

----------------------------------------
-- Fixity
pOpSymbol :: Parser String
pOpSymbol = lexeme . try $ do
  sym <- some (satisfy isOperatorChar)
  if sym `elem` ["->", "=", ":", "<-", "|", "\\", "→", "←"] || isReserved sym
    then fail $ "Reserved operator symbol: " ++ show sym
    else pure sym

pFixityDecl :: Parser Decl
pFixityDecl = do
  assoc <-
    choice
      [ AssocLeft <$ symbol "infixl"
      , AssocRight <$ symbol "infixr"
      , AssocNone <$ symbol "infix"
      ]

  prec <- fromInteger <$> pInteger
  ops <- pOpSymbol `sepBy1` symbol ","
  pure $ FixityDecl assoc prec ops

isOperatorChar :: Char -> Bool
isOperatorChar c =
  c `elem` ("!#$%&*+./<=> ?@\\^|-~" :: String)
    || generalCategory c
      `elem` [ MathSymbol
             , CurrencySymbol
             , ModifierSymbol
             , OtherSymbol
             , DashPunctuation
             , ConnectorPunctuation
             ]

buildOperatorTable :: FixityEnv -> [[Operator Parser Expr]]
buildOperatorTable env =
  buildLevel env
    <$> M.toDescList
      (M.foldrWithKey groupOp M.empty env)
 where
  groupOp :: String -> Fixity -> M.Map Int [String] -> M.Map Int [String]
  groupOp op (Fixity _assoc prec) = M.insertWith (++) prec [op]

  buildLevel :: FixityEnv -> (Int, [String]) -> [Operator Parser Expr]
  buildLevel fixEnv (_prec, ops) = makeOp fixEnv <$> ops

  makeOp :: FixityEnv -> String -> Operator Parser Expr
  makeOp fixEnv op =
    case M.lookup op fixEnv of
      Just (Fixity AssocLeft _) ->
        InfixL (mkBinOp op <$ symbol op) -- Left-associative infix
      Just (Fixity AssocRight _) ->
        InfixR (mkBinOp op <$ symbol op) -- Right-associative infix
      Just (Fixity AssocNone _) ->
        InfixN (mkBinOp op <$ symbol op) -- Non-associative infix
      Nothing ->
        InfixL (mkBinOp op <$ symbol op) -- Default to left-associative
  mkBinOp :: String -> Expr -> Expr -> Expr
  mkBinOp opStr =
    case M.lookup opStr builtinOps of
      Just op -> EBinOp op -- Built-in operator: use EBinOp
      Nothing -> ECustomOp opStr -- User-defined: use ECustomOp

parseExprWith :: FixityEnv -> Parser Expr
parseExprWith fixEnv = makeExprParser pApp (unaryOps : table)
 where
  table = buildOperatorTable fixEnv
  unaryOps = [Prefix (EUnaryOp Negate <$ symbol "-")]

parseExpr :: Parser Expr
parseExpr = parseExprWith defaultFixityEnv

----------------------------------------
-- Pattern parser

pConsPattern :: Parser Pat
pConsPattern = do
  name <- pCtorIdent
  pats <- many pPattern
  pure $ PCons name pats

pTuplePattern :: Parser Pat
pTuplePattern = enclosed '(' ')' $ do
  -- (x, y)
  pats <- sepBy pPattern (symbol ",")
  pure $ case pats of
    [p] -> p -- Just a parenthesized pattern
    ps -> PTuple ps -- A tuple pattern

pListPattern :: Parser Pat
pListPattern = enclosed '[' ']' $ do
  -- [x, y, z]
  pats <- sepBy pPattern (symbol ",")
  pure $ PList pats

pPartitionPattern :: Parser Pat
pPartitionPattern = enclosed '(' ')' $ do
  -- (x::xs)
  name <- pIdent <|> symbol "_"
  _ <- symbol "::"
  PPartition name <$> pPattern

pWildcardPattern :: Parser Pat
pWildcardPattern = PWildcard <$ symbol "_"

pPattern :: Parser Pat
pPattern =
  choice
    [ PLit <$> pLiteral
    , try pConsPattern
    , PVar <$> pIdent
    , try pPartitionPattern
    , try pTuplePattern
    , try pListPattern
    , pWildcardPattern
    ]

----------------------------------------
--  Declaration parsers

-- Parse a valid data constructor in the format of either:
--   - Nullary: ConstructorName T
--   - Some: ConstructorName T T' ...
--   - Tuple: ConstructorName (T, T', ...)
--   - Record: ConstructorName { field1: T, field2: T', ... }

pDataNullaryConstructor :: Parser (String, [Ty])
pDataNullaryConstructor = do
  name <- pCtorIdent
  pure (name, [])

pDataTupleConstructor :: Parser (String, [Ty])
pDataTupleConstructor = do
  name <- pCtorIdent
  tys <- enclosed '(' ')' (sepBy pType (symbol ","))
  pure (name, tys)

pDataTypedConstructor :: Parser (String, [Ty])
pDataTypedConstructor = do
  name <- pCtorIdent
  tys <- some pBaseTy
  pure (name, tys)

pDataRecordConstructor :: Parser (String, [Ty])
pDataRecordConstructor = do
  -- TODO: Preserve field names
  name <- pCtorIdent
  fields <- enclosed '{' '}' (sepBy parseField (symbol ","))
  let tys = map snd fields
  pure (name, tys)
 where
  parseField = do
    fieldName <- pIdent
    _ <- symbol ":"
    fieldType <- pType
    pure (fieldName, fieldType)

-- Product constructors
pDataConstructor :: Parser (String, [Ty])
pDataConstructor =
  choice
    [ try pDataTupleConstructor
    , try pDataTypedConstructor
    , try pDataRecordConstructor
    , pDataNullaryConstructor
    ]

pDataDeclaration :: Parser Decl
pDataDeclaration = do
  _ <- symbol "data"
  typeName <- pCtorIdent
  typeParams' <- many pIdent
  _ <- symbol "="
  constructors <- pDataConstructor `sepBy1` symbol "|"
  pure $ DataDecl typeName typeParams' constructors

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
    FunctionDecl (FunctionDeclaration name paramBinders mty lambdaBody)

pClassDeclaration :: Parser Decl
pClassDeclaration = do
  -- class ClassName a b where
  --   method1 : a -> b
  --   method2 : a -> a

  _ <- symbol "class"
  className' <- pCtorIdent
  typeParams' <- many pIdent
  _ <- symbol "where"
  methods' <- many parseMethod
  pure $ ClassDecl (TypeclassDeclaration className' typeParams' methods')
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
  className' <- pCtorIdent
  _ <- symbol ":"
  ty <- pType
  _ <- symbol "where"
  methods' <- many parseMethodImpl
  pure $ ClassImpl (TypeclassImplementation className' ty methods')
 where
  parseMethodImpl = do
    _ <- symbol "def"
    methodName <- pIdent
    methodExpr <- parseExpr
    pure (methodName, methodExpr)

parseDecl :: Parser Decl
parseDecl =
  choice
    [ try pFixityDecl
    , try pFunctionDeclaration
    , try pDataDeclaration
    , try pClassDeclaration
    , try pClassImplementation
    , -- Fallback to parsing an expression declaration
      ExprDecl <$> parseExpr
    ]

-- TODO: Add parse program that parses fixity declarations first, and then everything else

gatherFixityDecls :: [Decl] -> FixityEnv
gatherFixityDecls = foldr gatherFixity M.empty
 where
  gatherFixity :: Decl -> FixityEnv -> FixityEnv
  gatherFixity (FixityDecl assoc prec ops) env = foldr (\op -> M.insert op (Fixity assoc prec)) env ops

applyFixityDecl :: FixityEnv -> Decl -> FixityEnv
applyFixityDecl env (FixityDecl assoc prec ops) =
  foldr (\op acc -> M.insert op (Fixity assoc prec) acc) env ops
applyFixityDecl env _ = env

parseProgram :: Parser [Decl]
parseProgram = do
  let fixityDecls = gatherFixityDecls <$> many (try pFixityDecl)
  decls <- many (try parseDecl)
  pure decls