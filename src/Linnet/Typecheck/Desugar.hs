module Linnet.Typecheck.Desugar where

import Data.List (elemIndex)
import Linnet.AST.Core qualified as Core
import Linnet.AST.Declarations qualified as AST
import Linnet.AST.Operators (binOpToString, unOpToString)

-- Context used during desugaring to keep track of terms and types
data TypeContext = TypeContext
  { termEnv :: [String]
  , typeEnv :: [String]
  }
  deriving (Show, Eq)

-- | Create an empty typing context
mkContext :: TypeContext
mkContext = TypeContext{termEnv = [], typeEnv = []}

-- | Create a default typing context with built-in terms and types
defaultContext :: TypeContext
defaultContext =
  TypeContext
    { termEnv = ["Cons", "Nil"]
    , typeEnv = []
    }

try :: Maybe a -> String -> Either String a
try (Just x) _ = Right x
try Nothing err = Left err

lookupTerm :: TypeContext -> String -> Either String Int
lookupTerm ctx name = try (name `elemIndex` termEnv ctx) ("Unbound term variable: " ++ name)

lookupType :: TypeContext -> String -> Either String Int
lookupType ctx name = try (name `elemIndex` typeEnv ctx) ("Unbound type variable: " ++ name)

-- Desugaring

-- | Desugar a type from the surface AST to the core AST
desugarTy :: TypeContext -> AST.Ty -> Either String Core.Ty
desugarTy _ctx AST.TInt = Right Core.TInt
desugarTy _ctx AST.TFloat = Right Core.TFloat
desugarTy _ctx AST.TBool = Right Core.TBool
desugarTy _ctx AST.TString = Right Core.TString
desugarTy _ctx AST.TUnit = Right Core.TUnit
desugarTy ctx (AST.TVar name) = Core.TVar <$> lookupType ctx name
desugarTy ctx (AST.TFn arg ret) = Core.TFn <$> desugarTy ctx arg <*> desugarTy ctx ret
desugarTy ctx (AST.TForall var ty) = Core.TForall <$> desugarTy newCtx ty
 where
  newCtx = ctx{typeEnv = var : typeEnv ctx}
desugarTy ctx (AST.TCons name params) = do
  paramTys <- traverse (desugarTy ctx) params
  Right $ Core.TCons name paramTys

-- | Desugar an expression from the surface AST to the core AST
desugarExpr :: TypeContext -> AST.Expr -> Either String Core.Expr
desugarExpr ctx expr = case expr of
  AST.ELit lit -> Right $ Core.ELit lit
  AST.EUnit -> Right Core.EUnit
  AST.EVar name -> Core.EVar <$> lookupTerm ctx name
  AST.EUnaryOp op e -> do
    -- Look up Debrujin index for the operator and apply it
    index <- lookupTerm ctx (unOpToString op)
    desugaredExpr <- desugarExpr ctx e
    Right $ Core.EApp (Core.EVar index) desugaredExpr
  AST.EBinOp op left right -> do
    desugaredLeft <- desugarExpr ctx left
    desugaredRight <- desugarExpr ctx right

    -- Look up Debrujin index for the operator and apply it
    index <- lookupTerm ctx (binOpToString op)
    Right $ Core.EApp (Core.EApp (Core.EVar index) desugaredLeft) desugaredRight
  AST.EList xs -> desugarList ctx xs
  AST.ETuple xs -> desugarTuple ctx xs
  -- Catamorphism for lambda expressions: extend content
  AST.ELam params body -> do
    -- Extend context with parameter names
    let newCtx = ctx{termEnv = params ++ termEnv ctx}
    desugaredBody <- desugarExpr newCtx body
    Right $ foldr (\_ acc -> Core.ELam Core.TUnit acc) desugaredBody params
  -- Function application
  AST.EApp f x -> do
    desugaredF <- desugarExpr ctx f
    desugaredX <- desugarExpr ctx x
    Right $ Core.EApp desugaredF desugaredX
  AST.ELet (AST.Binder name mty) val body -> do
    desugaredVal <- desugarExpr ctx val
    ty <- case mty of
      Just t -> desugarTy ctx t -- Desugar type annotation if present
      Nothing -> Right Core.TUnit

    -- Extend context with new variable
    let newCtx = ctx{termEnv = name : termEnv ctx}
    desugaredBody <- desugarExpr newCtx body
    Right $ Core.ELet ty desugaredVal desugaredBody

  --
  _ -> Left "Desugaring of expressions not implemented yet"

desugarList :: TypeContext -> [AST.Expr] -> Either String Core.Expr
desugarList ctx = expand
 where
  expand [] = do
    nilIndex <- lookupTerm ctx "Nil"
    Right $ Core.EApp (Core.EVar nilIndex) Core.EUnit
  expand (x : xs) = do
    desugarHead <- desugarExpr ctx x
    desugarTail <- expand xs
    consIndex <- lookupTerm ctx "Cons"
    Right $ Core.EApp (Core.EApp (Core.EVar consIndex) desugarHead) desugarTail

desugarTuple :: TypeContext -> [AST.Expr] -> Either String Core.Expr
desugarTuple = desugarList
