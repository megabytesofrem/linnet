{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Linnet.Typecheck.Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import Linnet.AST.Core qualified as Core

-- Mapper type used internally for type shifting/substitution
type Mapper a = a -> a -> Core.Ty

-- Type environment mapping Debrujin indices to types
type TypeEnv = [Core.Ty]

--  Typechecking monad with access to the type environment
newtype TypecheckM a = TypecheckM
  { runTypecheckM :: ReaderT TypeEnv (Except String) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadReader TypeEnv TypecheckM where
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
withType :: Core.Ty -> TypecheckM a -> TypecheckM a
withType ty = local (ty :)

-- Lookup a type variable in the environment
lookupEnv :: Int -> TypecheckM Core.Ty
lookupEnv idx = do
  env <- ask
  if idx < length env
    then return $ env !! idx
    else throwError $ "Unbound type variable: " ++ show idx

-- This is the core of System F.
-- Given an expression, this function will check it, and return its type.
-- It is not an inference function, it is a checking function. Inference will come later.

typeOf :: Core.Expr -> TypecheckM Core.Ty
typeOf _ = throwError "Typechecking not implemented yet"

-- | Run the typechecker with a given type environment
runTypecheck :: TypeEnv -> TypecheckM a -> Either String a
runTypecheck env (TypecheckM m) = runExcept (runReaderT m env)
