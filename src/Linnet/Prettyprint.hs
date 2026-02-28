{-# LANGUAGE InstanceSigs #-}

module Linnet.Prettyprint where

import Control.Monad (forM)
import Control.Monad.State
import Data.List (intercalate)
import Linnet.AST.Declarations
import Linnet.AST.Operators

-- State for the pretty printer
newtype PrettyprintState = PrettyprintState
  { indentLevel :: Int
  }

-- Prettyprinter monad to keep track of indentation and state
type Prettyprinter a = State PrettyprintState a

prettyPrintWith
  :: (Prettyprint a, Traversable t)
  => (a -> Prettyprinter String)
  -> t a
  -> Prettyprinter (t String)
prettyPrintWith = traverse

prettyPrintFoldable
  :: (Prettyprint a, Traversable t)
  => t a
  -> Prettyprinter (t String)
prettyPrintFoldable = traverse pretty

indentsToSpaces :: PrettyprintState -> String
indentsToSpaces p = replicate (indentLevel p * 2) ' '

getIndent :: Prettyprinter String
getIndent = gets indentsToSpaces

withIndent :: Prettyprinter String -> Prettyprinter String
withIndent action = do
  oldLevel <- gets indentLevel
  modify $ \s -> s{indentLevel = oldLevel + 1}
  result <- action
  modify $ \s -> s{indentLevel = oldLevel}
  pure result

-- Typeclass for pretty printing AST nodes
class Prettyprint a where
  pretty :: a -> Prettyprinter String

instance Prettyprint Ty where
  pretty TInt = pure "Int"
  pretty TFloat = pure "Float"
  pretty TBool = pure "Bool"
  pretty TString = pure "String"
  pretty TUnit = pure "()"
  pretty (TFn arg ret) = do
    argStr <- case arg of
      TFn _ _ -> (\s -> "(" <> s <> ")") <$> pretty arg
      _ -> pretty arg
    retStr <- pretty ret
    pure $ argStr <> " -> " <> retStr
  pretty (TVar name) = pure name
  pretty (TForall var ty) = do
    tyStr <- pretty ty
    pure $ "forall " <> var <> ". " <> tyStr
  pretty (TCons name params) = do
    paramStr <- prettyPrintFoldable params
    pure $ name <> "[" <> intercalate ", " paramStr <> "]"

instance Prettyprint UnaryOp where
  pretty Negate = pure "-"

instance Prettyprint BinOp where
  pretty Add = pure "+"
  pretty Sub = pure "-"
  pretty Mul = pure "*"
  pretty Div = pure "/"
  pretty Mod = pure "%"
  pretty Eq = pure "=="
  pretty Neq = pure "/="
  pretty Lt = pure "<"
  pretty Gt = pure ">"
  pretty LtEq = pure "<="
  pretty GtEq = pure ">="
  -- \* Functional operators
  pretty Apply = pure " "
  pretty Compose = pure "."
  pretty Bind = pure ">>="
  pretty Pipe = pure ">>"
  pretty Map = pure "<$>"
  pretty UFO = pure "<*>"
  pretty Alt = pure "<|>"

instance Prettyprint Literal where
  pretty (LitInt n) = pure $ show n
  pretty (LitFloat f) = pure $ show f
  pretty (LitBool b) = pure $ show b
  pretty (LitString s) = pure $ show s

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

-- -- * Declarations
instance Prettyprint Decl where
  pretty (ExprDeclaration expr) = pretty expr
  pretty (ClassDeclaration classDecl) = pretty classDecl
  pretty (ClassImplementation impl) = pretty impl
  pretty (DataDeclaration name params constructors) = pure ""

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

-- Evaluate a single step of the pretty printer and return the resulting string
evalPrettyPrint :: (Prettyprint a) => a -> String
evalPrettyPrint item = evalState (pretty item) (PrettyprintState 0)

-- Run the pretty printer and return the resulting string along with the final state
runPrettyPrint :: (Prettyprint a) => a -> PrettyprintState -> (String, PrettyprintState)
runPrettyPrint item = runState (pretty item)