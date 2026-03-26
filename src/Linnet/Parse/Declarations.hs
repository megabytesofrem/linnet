module Linnet.Parse.Declarations
  ( -- * Expression parsers
    pList
  , pTuple
  , pBinder
  , pApp
  , pLambda
  , pLet
  , pIf
  , pMatch
  , pMatchBranch
  -- Monadic bindings
  , pMonadBind
  , pMonadLet
  , pMonadDo
  , parseExpr

    -- * Declaration parsers
  , pDataDeclaration
  , pFunctionDeclaration
  , pClassDeclaration
  , pClassImplementation
  , parseDecl

    -- * Top level parser
  , parseProgram
  )
where

import Control.Monad (unless)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State
import Data.Char (GeneralCategory (..), generalCategory)
import Data.List (intercalate)
import Data.Map.Strict qualified as M
import Linnet.AST
import Linnet.Parse.Lexer
import Linnet.Parse.Pattern
import Linnet.Parse.Types
import Text.Megaparsec

-----------------------------------------
-- Expression parsers

-- Lists and tuples: [1, 2, 3],  (1, 2, 3)
pList :: Parser Expr
pList = enclosed '[' ']' (EList <$> parseExpr `sepBy` symbol ",")

pTuple :: Parser Expr
pTuple = enclosed '(' ')' (ETuple <$> parseExpr `sepBy` symbol ",")

-- Variable binder parser for lambda abstractions and bindings
pBinder :: Parser Binder
pBinder = do
  name <- pIdent
  ty <- optional (symbol ":" >> pType)
  pure $ Binder name ty

-- Function application: f x, f x y, etc.
pApp :: Parser Expr
pApp = do
  f <- pTerm
  args <- many pTerm
  pure $ foldl EApp f args

pLambda :: Parser Expr
pLambda = do
  _ <- symbol "\\"
  binders <- some pIdent
  _ <- altSym "->" "→"
  ELam binders <$> parseExpr

pLet :: Parser Expr
pLet = do
  _ <- symbol "let"
  binder <- pBinder
  _ <- symbol "="
  value <- parseExpr
  _ <- symbol "in"
  ELet binder value <$> parseExpr

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- parseExpr
  _ <- symbol "then"
  thenBranch <- parseExpr
  _ <- symbol "else"
  EIf cond thenBranch <$> parseExpr

pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  expr <- parseExpr
  _ <- symbol "with"
  branches <- some pMatchBranch
  pure $ EMatch expr branches

pMatchBranch :: Parser (Pat, Expr)
pMatchBranch = do
  _ <- symbol "|"
  pat <- pPattern
  _ <- altSym "->" "→"
  (pat,) <$> parseExpr

-----------------------------------------
-- Monadic bindings

pMonadBind :: Parser Expr
pMonadBind = do
  ident <- pIdent
  _ <- altSym "<-" "←"
  EBind ident <$> parseExpr

pMonadLet :: Parser Expr
pMonadLet = do
  _ <- symbol "let!"
  binder <- pBinder
  _ <- altSym "<-" "←"
  ELetM binder <$> parseExpr

pMonadDo :: Parser Expr
pMonadDo = do
  exprs <- enclosedStr "do" "end" (validExpr `sepByTokenOrEOL` ";")
  pure $ EBlock exprs
 where
  -- A valid expression in a monad block can be either let!, binding, or a normal expression
  validExpr = pMonadLet <|> pMonadBind <|> parseExpr

-----------------------------------------

pTerm :: Parser Expr
pTerm =
  choice
    [ -- Parse monadic blocks first to allow let! and bind syntax inside them
      pMonadDo
    , pLet
    , pIf
    , pMatch
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

-----------------------------------------
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
  c `elem` ("!#$%&*+./<=>?@\\^|-~" :: String)
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

parseExprWith' :: FixityEnv -> Parser Expr
parseExprWith' = parseExprWith

---------------------------------
-- Declaration parsers
---------------------------------
-- Nullary constructors: data Null = Null
-- Tuple constructors: (Int, String), etc.
-- Product constructors: data Maybe a = Just a | Nothing
-- Record constructors: data Person = Person { name: String, age: Int }

pDataNullaryConstructor :: Parser (String, [Ty])
pDataNullaryConstructor = do
  name <- pCtorName
  pure (name, [])

pDataTupleConstructor :: Parser (String, [Ty])
pDataTupleConstructor = do
  name <- pCtorName
  tys <- enclosed '(' ')' (sepBy pType (symbol ","))
  pure (name, tys)

pDataTypedConstructor :: Parser (String, [Ty])
pDataTypedConstructor = do
  name <- pCtorName
  tys <- some pAtomTy
  pure (name, tys)

pDataRecordConstructor :: Parser (String, [Ty])
pDataRecordConstructor = do
  -- TODO: Preserve field names
  name <- pCtorName
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
  typeName <- pCtorName
  typeParams' <- many pIdent
  _ <- symbol "="

  first <- optional (symbol "|") >> pDataConstructor
  rest <- many (symbol "|" >> pDataConstructor)
  pure $ DataDecl typeName typeParams' (first : rest)

pFunctionName :: Parser String
pFunctionName = lexeme (pIdent <|> pOpSymbol) <?> "function or operator name"

pFunctionSignature :: Parser (String, [Binder], Maybe Ty)
pFunctionSignature = do
  -- name : a -> b -> c
  name <- pFunctionName
  _ <- symbol ":"
  ty <- pType
  pure (name, [], Just ty) -- Fill this in when parsing the full declaration

pFunctionDeclaration :: FixityEnv -> Parser Decl
pFunctionDeclaration env = do
  -- name : a -> b -> c
  -- def name x y = expr
  -- def name x y | pattern -> expr

  (sigName, _sigParams, mty) <- pFunctionSignature
  _ <- symbol "def"
  defName <- pFunctionName
  unless (defName == sigName) $
    fail $
      "Function name mismatch: signature is "
        ++ show sigName
        ++ " but definition is "
        ++ show defName

  -- Parse parameters until we hit an equals sign or a pipe (for pattern matching)
  paramBinders <- manyTill pBinder (lookAhead (symbol "=" <|> symbol "|"))

  -- Body can either be a simple expression, or a pattern match with branches
  body <- pSimpleBody <|> pPatternBody

  let paramNames = map (\(Binder n _) -> n) paramBinders
  pure $ FunctionDecl (FunctionDeclaration sigName paramBinders mty (wrapBody paramNames body))
 where
  pSimpleBody :: Parser FunctionBody
  pSimpleBody = do
    _ <- symbol "="
    SimpleBody <$> parseExprWith' env

  pPatternBody :: Parser FunctionBody
  pPatternBody = do
    branches <- some pMatchBranch
    pure $ PatternBody branches

  wrapBody :: [String] -> FunctionBody -> FunctionBody
  wrapBody [] body = body
  wrapBody params (SimpleBody expr) = SimpleBody $ foldr (\p acc -> ELam [p] acc) expr params
  wrapBody _ body@(PatternBody _branches) = body

pClassDeclaration :: Parser Decl
pClassDeclaration = do
  -- class ClassName a b where
  --   method1 : a -> b
  --   method2 : a -> a

  _ <- symbol "class"
  className' <- pCtorName
  typeParams' <- many pIdent
  _ <- symbol "where"
  methods' <- many parseMethod
  pure $ ClassDecl (TypeclassDeclaration className' typeParams' methods')
 where
  parseMethod = do
    methodName <- pIdent <|> pOpSymbol
    _ <- symbol ":"
    methodType <- pType
    pure (methodName, [], methodType) -- No method type parameters for now

pClassImplementation :: Parser Decl
pClassImplementation = do
  -- impl ClassName MyType where
  --   def method1 a b = ...
  --   def method2 a b = ...

  _ <- symbol "impl"
  className' <- pCtorName
  ty <- pType
  _ <- symbol "where"
  methods' <- many parseMethodImpl
  pure $ ClassImpl (TypeclassImplementation className' ty methods')
 where
  parseMethodImpl = do
    _ <- symbol "def"
    methodName <- pIdent <|> pOpSymbol
    methodExpr <- parseExpr
    pure (methodName, methodExpr)

-- Parse an import declaration of the form:
-- Qualified: import Data.List as List
-- Exposing: import Data.List (map, filter)
-- Total: import Data.List

pQualifiedImport :: Parser Decl
pQualifiedImport = do
  _ <- symbol "import"
  moduleName <- pModuleName
  _ <- symbol "as"
  ImportDecl moduleName . Just <$> pModuleName

pExposingImport :: Parser Decl
pExposingImport = do
  _ <- symbol "import"
  moduleName <- pModuleName
  _ <- symbol "("
  exposed <- pIdent `sepBy1` symbol ","
  _ <- symbol ")"
  pure $ ImportDeclExposing moduleName exposed

pTotalImport :: Parser Decl
pTotalImport = do
  _ <- symbol "import"
  moduleName <- pModuleName

  -- If there is no alias given, we can access it via its full name
  pure $ ImportDecl moduleName Nothing

pImportDecl :: Parser Decl
pImportDecl = try pQualifiedImport <|> try pExposingImport <|> try pTotalImport

parseDecl :: FixityEnv -> Parser Decl
parseDecl env =
  choice
    [ try pFixityDecl
    , try (pFunctionDeclaration env)
    , try pDataDeclaration
    , try pClassDeclaration
    , try pClassImplementation
    , try pImportDecl
    , -- Fallback to parsing an expression declaration
      ExprDecl <$> parseExprWith' env
    ]

applyFixityDecl :: FixityEnv -> Decl -> FixityEnv
applyFixityDecl env (FixityDecl assoc prec ops) =
  foldr (\op acc -> M.insert op (Fixity assoc prec) acc) env ops
applyFixityDecl env _ = env

-- parseDecl *but* we carry state for the fixity environment to update as we parse fixities
parseDeclS :: ParserS Decl
parseDeclS = do
  lift sc
  env <- get
  decl <- lift . parseDecl $ env
  case decl of
    FixityDecl{} -> modify (`applyFixityDecl` decl) -- Update state with new fixity info
    _ -> pure ()

  pure decl

parseProgram :: Parser [Decl]
parseProgram = evalStateT (many parseDeclS) defaultFixityEnv