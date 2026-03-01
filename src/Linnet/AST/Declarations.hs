{-# LANGUAGE TemplateHaskell #-}

module Linnet.AST.Declarations
  ( Ty (..)

    -- * AST
  , Literal (..)
  , Expr (..)
  , Decl (..)
  , TypeclassDeclaration (..)
  , TypeclassImplementation (..)
  , FunctionDeclaration (..)

    -- * Binders
  , Binder (..)
  , LoopBinder (..)
  )
where

import Control.Lens.TH (makeLenses)
import Control.Monad (forM)
import Data.List (intercalate)
import Linnet.AST.Operators (BinOp, UnaryOp)
import Linnet.AST.Pattern (Pat)
import Linnet.AST.Types (Literal (..), Ty (..))
import Linnet.Prettyprint (Prettyprint (..), getIndent, prettyPrintFoldable, withIndent)

-- Binder used for let-bindings and function parameter types
data Binder = Binder String (Maybe Ty)
  deriving (Show, Eq)

-- Binder used for loops, which can have a type annot and an initial value
data LoopBinder = LoopBinder String (Maybe Ty) Expr
  deriving (Show, Eq)

{- FOURMOLU_DISABLE -}
data Expr
  = ELit Literal
  | EUnit -- ()
  | EIdent String
  | EUnaryOp UnaryOp Expr
  | EBinOp BinOp Expr Expr
  | EList [Expr] -- [1, 2, 3]
  | ETuple [Expr] -- (1, 2, 3)
  | ELam [String] Expr -- \x y -> expr
  | EApp Expr Expr -- f x y z
  | ELet Binder Expr Expr -- let x = expr1 in expr2
  | EIf Expr Expr Expr -- if cond then expr1 else expr2
  | EMatch Expr [(Pat, Expr)] -- match expr with | pat -> expr
  | ELoop [LoopBinder] Expr -- loop (binders) expr
  
  -- Monadic binding and blocks
  | ELetM Binder Expr -- let! x = expr
  | EBind String Expr -- x <- m
  | EBlock [Expr]     -- do ... end blocks
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

----------------------------------------
-- Declarations

type DataTypeConstructors = [(String, [Ty])]

data TypeclassDeclaration = TypeclassDecl
  { className :: String
  , typeParams :: [String]
  , methods :: [(String, [String], Ty)]
  }
  deriving (Show, Eq)

makeLenses ''TypeclassDeclaration

data TypeclassImplementation = TypeclassImpl
  { className :: String
  , implType :: Ty
  , methods :: [(String, Expr)]
  }
  deriving (Show, Eq)

makeLenses ''TypeclassImplementation

data FunctionDeclaration = FunctionDecl
  { funcName :: String
  , funcParams :: [Binder]
  , funcReturnType :: Maybe Ty
  , funcBody :: Expr
  }
  deriving (Show, Eq)

makeLenses ''FunctionDeclaration

data Decl
  = ExprDeclaration Expr
  | FunctionDeclaration FunctionDeclaration
  | DataDeclaration String [String] DataTypeConstructors
  | ClassDeclaration TypeclassDeclaration
  | ClassImplementation TypeclassImplementation
  deriving (Show, Eq)

-- Prettyprint

instance Prettyprint Binder where
  pretty (Binder name mty) = do
    tyStr <- case mty of
      Just ty -> do
        tyStr' <- pretty ty
        pure $ ": " <> tyStr'
      Nothing -> pure ""
    pure $ name <> tyStr

instance Prettyprint LoopBinder where
  pretty (LoopBinder name mty initVal) = do
    tyStr <- case mty of
      Just ty -> do
        tyStr' <- pretty ty
        pure $ ": " <> tyStr'
      Nothing -> pure ""
    initStr <- do
      initStr' <- pretty initVal
      pure $ " = " <> initStr'
    pure $ name <> tyStr <> initStr

instance Prettyprint Expr where
  pretty (ELit lit) = pretty lit
  pretty EUnit = pure "()"
  pretty (EIdent name) = pure name
  pretty (EUnaryOp op expr) = do
    opStr <- pretty op
    exprStr <- pretty expr
    pure $ opStr <> exprStr
  pretty (EBinOp op left right) = do
    leftStr <- pretty left
    opStr <- pretty op
    rightStr <- pretty right
    pure $ leftStr <> " " <> opStr <> " " <> rightStr
  pretty (EList elems) = do
    elemsStr <- prettyPrintFoldable elems
    pure $ "[" <> intercalate ", " elemsStr <> "]"
  pretty (ETuple elems) = do
    elemsStr <- prettyPrintFoldable elems
    pure $ "(" <> intercalate ", " elemsStr <> ")"
  pretty (ELam params body) = do
    bodyStr <- pretty body
    pure $ "\\" <> unwords params <> " -> " <> bodyStr
  pretty (EApp func arg) = do
    funcStr <- pretty func
    argStr <- pretty arg
    pure $ funcStr <> " " <> argStr
  pretty (ELet binder val body) = do
    binderStr <- pretty binder
    valStr <- pretty val
    bodyStr <- pretty body
    pure $ "let " <> binderStr <> " = " <> valStr <> " in " <> bodyStr
  pretty (EIf cond thenBranch elseBranch) = do
    condStr <- pretty cond
    thenStr <- pretty thenBranch
    elseStr <- pretty elseBranch
    pure $ "if " <> condStr <> " then " <> thenStr <> " else " <> elseStr
  -- pretty (EMatch expr cases) = do
  --   exprStr <- pretty expr
  --   casesStr <- withIndent $ do
  --     indent <- getIndent
  --     casesStr <- forM cases $ \(pat, caseExpr) -> do
  --       patStr <- pretty pat
  --       caseExprStr <- pretty caseExpr
  --       pure $ indent <> "| " <> patStr <> " -> " <> caseExprStr
  --     pure $ intercalate "\n" casesStr
  --   baseIndent <- getIndent
  --   pure $ "match " <> exprStr <> " with\n" <> casesStr <> "\n" <> baseIndent
  pretty (ELoop binders body) = do
    bindersStr <- prettyPrintFoldable binders
    bodyStr <- pretty body
    pure $ "loop (" <> intercalate ", " bindersStr <> ")" <> "{" <> bodyStr <> "}"
  pretty (ELetM binder val) = do
    binderStr <- pretty binder
    valStr <- pretty val
    pure $ "let! " <> binderStr <> " = " <> valStr
  pretty (EBind name val) = do
    valStr <- pretty val
    pure $ name <> " <- " <> valStr
  pretty (EBlock exprs) = do
    exprsStr <- prettyPrintFoldable exprs
    pure $ "!{ " <> intercalate "; " exprsStr <> " }"
  pretty _ = pure "Not implemented yet"

-- -- * Declarations
instance Prettyprint Decl where
  pretty (ExprDeclaration expr) = pretty expr
  pretty (ClassDeclaration classDecl) = pretty classDecl
  pretty (ClassImplementation impl) = pretty impl
  pretty (DataDeclaration name params constructors) = pure "Not implemented yet"
  pretty (FunctionDeclaration funcDecl) = pure "Not implemented yet"

instance Prettyprint TypeclassDeclaration where
  -- Pretty print a typeclass declaration:
  -- class Show[a] {
  --   fn show[a] : a -> String
  -- }
  pretty (TypeclassDecl name params methods') = do
    let paramsStr = if null params then "" else "[" <> intercalate ", " params <> "]"
    methodsStr <- withIndent $ do
      indent <- getIndent
      methodsStr <- forM methods' $ \(methodName, methodParams, methodType) -> do
        typeStr <- pretty methodType
        let paramsStr' =
              if null methodParams
                then ""
                else "[" <> intercalate ", " methodParams <> "]"
        pure $ indent <> "fn " <> methodName <> paramsStr' <> " -> " <> typeStr
      pure $ intercalate "\n" methodsStr
    baseIndent <- getIndent
    pure $ "class " <> name <> paramsStr <> " {\n" <> methodsStr <> "\n" <> baseIndent <> "}"

instance Prettyprint TypeclassImplementation where
  -- Pretty print a typeclass implementation:
  -- impl Show : MyType {
  --  fn show (x: MyType) -> String = ..
  -- }
  pretty (TypeclassImpl cname ty methods') = do
    tyStr <- pretty ty
    methodsStr <- withIndent $ do
      indent <- getIndent
      methodsStr <- forM methods' $ \(methodName, methodExpr) -> do
        methodExprStr <- pretty methodExpr
        pure $ indent <> "fn " <> methodName <> " = " <> methodExprStr
      pure $ intercalate "\n" methodsStr
    baseIndent <- getIndent
    pure $ "impl " <> cname <> " : " <> tyStr <> " {\n" <> methodsStr <> "\n" <> baseIndent <> "}"