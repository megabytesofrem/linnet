{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- HLINT ignore "Avoid lambda" -}

module Linnet.Typecheck.Typechecker where

import Control.Monad (mapAndUnzipM, unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (findIndex)
import Linnet.AST qualified as AST
import Linnet.AST.Core qualified as Core

-- Mapper type used internally for type shifting/substitution
type Mapper a = a -> a -> Core.Ty

data TypecheckEnv = TypecheckEnv
  { typeEnv :: [Core.Ty] -- Type environment for type variables
  , termEnv :: [(String, Core.Ty)] -- Term environment for variable names and their types
  }
  deriving (Show, Eq)

--  Typechecking monad with access to the type environment
newtype TypecheckM a = TypecheckM
  { runTypecheckM :: ReaderT TypecheckEnv (Except String) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadReader TypecheckEnv TypecheckM where
  ask = TypecheckM ask
  local f (TypecheckM m) = TypecheckM (local f m)

instance MonadError String TypecheckM where
  throwError = TypecheckM . throwError
  catchError (TypecheckM m) handler = TypecheckM $ catchError m (runTypecheckM . handler)

instance MonadFail TypecheckM where
  fail = throwError

--

tyMap :: Mapper Int -> Int -> Core.Ty -> Core.Ty
tyMap f = walk
 where
  -- Recursively walk the type, applying the mapping function to type variables
  walk c t = case t of
    Core.TVar idx -> f c idx
    Core.TFn arg ret -> Core.TFn (walk c arg) (walk c ret)
    Core.TForall body -> Core.TForall (walk (c + 1) body)
    Core.TCons name params -> Core.TCons name (map (walk c) params)
    _ -> t

tyShift :: Int -> Core.Ty -> Core.Ty
tyShift d = tyMap shiftVar d
 where
  shiftVar c idx
    -- If the Debrujin index >= the cutoff, shift it by d
    | idx >= c = Core.TVar (idx + d)
    | otherwise = Core.TVar idx

tySubst :: Int -> Core.Ty -> Core.Ty -> Core.Ty
tySubst j s = tyMap substVar j
 where
  substVar c idx
    -- If the index == j + c, shift s by c and substitute it
    | idx == j + c = tyShift c s
    -- If the index is greater than j + c, shift it down by 1. Adjust for the removed binder.
    | idx > j + c = Core.TVar (idx - 1)
    | otherwise = Core.TVar idx

-- Extend the typing environment with a new type and run a new typechecking action
-- in that environment.
checkWithType :: Core.Ty -> TypecheckM a -> TypecheckM a
checkWithType ty = local (\env -> env{typeEnv = ty : typeEnv env})

checkWithTerm :: String -> Core.Ty -> TypecheckM a -> TypecheckM a
checkWithTerm name ty = local (\env -> env{termEnv = (name, ty) : termEnv env})

-- Lookup a type variable in the environment
lookupType :: Int -> TypecheckM Core.Ty
lookupType idx = do
  env <- ask
  if idx < length (typeEnv env)
    then pure $ typeEnv env !! idx
    else throwError $ "Unbound type variable: " ++ show idx

lookupTerm :: String -> TypecheckM (Int, Core.Ty)
lookupTerm name = do
  env <- ask
  case findIndex (\(n, _) -> n == name) (termEnv env) of
    -- Make this not partial
    Just idx -> pure (idx, snd $ termEnv env !! idx)
    Nothing -> throwError $ "Unbound term variable: " ++ name

-- This is the core of System F.
-- Given an expression, this function will check it, and return its type.
-- It is not an inference function, it is a checking function. Inference will come later.

infer :: AST.Expr -> TypecheckM (Core.Ty, Core.Expr)
infer expr = case expr of
  AST.ELit lit -> inferLit lit
  AST.EUnit -> pure (Core.TUnit, Core.EUnit)
  AST.EVar name -> do
    (idx, ty) <- lookupTerm name
    pure (ty, Core.EVar idx)
  AST.EUnaryOp op e -> do
    (ty, e') <- infer e
    -- Here we would check the operator and the operand type to determine the result type
    throwError $ "Unary operator type inference not implemented for operator: " ++ show op
  AST.EBinOp op left right -> do
    (leftTy, left') <- infer left
    (rightTy, right') <- infer right
    throwError $ "Binary operator type inference not implemented for operator: " ++ show op
  AST.EList [] -> pure (Core.TCons "List" [], Core.EUnit) -- Empty list
  AST.EList (x : xs) -> do
    (headTy, headExpr) <- infer x
    (tys, exprs) <- mapAndUnzipM infer xs

    -- Check each type
    mapM_ (\ty -> unless (ty == headTy) (throwError "List element type mismatch")) tys
    let listTy = Core.TCons "List" [headTy]
    listExpr <- buildList (headExpr : exprs)
    pure (listTy, listExpr)
  AST.ETuple [] -> pure (Core.TCons "Tuple" [], Core.EUnit) -- Empty tuple
  AST.ETuple (x : xs) -> do
    (headTy, headExpr) <- infer x
    (tys, exprs) <- mapAndUnzipM infer xs

    -- Check each type
    mapM_ (\ty -> unless (ty == headTy) (throwError "Tuple element type mismatch")) tys
    let tupleTy = Core.TCons "Tuple" [headTy]
    tupleExpr <- buildTuple (headExpr : exprs)
    pure (tupleTy, tupleExpr)
  _ -> throwError $ "Type inference not implemented for expression: " ++ show expr

inferLit :: AST.Literal -> TypecheckM (Core.Ty, Core.Expr)
inferLit lit = case lit of
  AST.LitInt _ -> pure (Core.TInt, Core.ELit lit)
  AST.LitFloat _ -> pure (Core.TFloat, Core.ELit lit)
  AST.LitBool _ -> pure (Core.TBool, Core.ELit lit)
  AST.LitString _ -> pure (Core.TString, Core.ELit lit)

check :: AST.Expr -> Core.Ty -> TypecheckM Core.Expr
check expr expectedTy = do
  (inferredTy, inferredExpr) <- infer expr
  if inferredTy == expectedTy
    then pure inferredExpr
    else throwError $ "Type error: expected " ++ show expectedTy ++ ", but got " ++ show inferredTy

buildList :: [Core.Expr] -> TypecheckM Core.Expr
buildList exprs = do
  (consIdx, _) <- lookupTerm "Cons"
  (nilIdx, _) <- lookupTerm "Nil"
  pure $
    foldr
      (\e acc -> Core.EApp (Core.EApp (Core.EVar consIdx) e) acc)
      (Core.EVar nilIdx)
      exprs

buildTuple :: [Core.Expr] -> TypecheckM Core.Expr
buildTuple [] = pure Core.EUnit
buildTuple [e] = pure e
buildTuple (e : es) = do
  tupleRest <- buildTuple es
  (tupleIdx, _) <- lookupTerm "Tuple"
  pure $ Core.EApp (Core.EApp (Core.EVar tupleIdx) e) tupleRest

-- | Run the typechecker with a given type environment
runTypecheck :: TypecheckEnv -> TypecheckM a -> Either String a
runTypecheck env (TypecheckM m) = runExcept (runReaderT m env)
