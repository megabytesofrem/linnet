{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- HLINT ignore "Avoid lambda" -}

module Linnet.Typecheck.Typechecker where

import Control.Monad (mapAndUnzipM, unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List (elemIndex, findIndex)
import Linnet.AST qualified as AST
import Linnet.AST.Core qualified as Core

opSignature :: AST.BinOp -> Maybe (Core.Ty, Core.Ty, Core.Ty)
opSignature op = case op of
  AST.Add -> Just (Core.TInt, Core.TInt, Core.TInt)
  AST.Sub -> Just (Core.TInt, Core.TInt, Core.TInt)
  AST.Mul -> Just (Core.TInt, Core.TInt, Core.TInt)
  AST.Div -> Just (Core.TInt, Core.TInt, Core.TInt)
  AST.Mod -> Just (Core.TInt, Core.TInt, Core.TInt)
  AST.Eq -> Just (Core.TInt, Core.TInt, Core.TBool)
  AST.Neq -> Just (Core.TInt, Core.TInt, Core.TBool)
  AST.Lt -> Just (Core.TInt, Core.TInt, Core.TBool)
  AST.Gt -> Just (Core.TInt, Core.TInt, Core.TBool)
  AST.LtEq -> Just (Core.TInt, Core.TInt, Core.TBool)
  AST.GtEq -> Just (Core.TInt, Core.TInt, Core.TBool)
  _ -> Nothing

-- Mapper type used internally for type shifting/substitution
type Mapper a = a -> a -> Core.Ty

data TypecheckEnv = TypecheckEnv
  { typeEnv :: [Core.Ty] -- Type environment for type variables
  , typeVarNames :: [String]
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

defaultEnv :: TypecheckEnv
defaultEnv =
  TypecheckEnv
    { typeEnv = []
    , typeVarNames = []
    , -- The absolute bare minimum for the term environment
      termEnv =
        [ ("Cons", Core.TForall (Core.TFn (Core.TVar 0) (Core.TFn (Core.TCons "List" [Core.TVar 0]) (Core.TCons "List" [Core.TVar 0]))))
        , ("Nil", Core.TForall (Core.TCons "List" [Core.TVar 0]))
        , ("Tuple", Core.TForall (Core.TFn (Core.TVar 0) (Core.TFn Core.TUnit (Core.TCons "Tuple" [Core.TVar 0]))))
        , -- Boolean
          ("True", Core.TCons "Bool" [])
        , ("False", Core.TCons "Bool" [])
        ]
    }

----------------------------------------
-- System F type substitution and shifting

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

--

-- Extend the typing environment with a new type and run a new typechecking action
-- in that environment.
checkWithType :: Core.Ty -> TypecheckM a -> TypecheckM a
checkWithType ty = local (\env -> env{typeEnv = ty : typeEnv env})

-- Extend the term environment with a new variable and run a new typechecking action
-- in that environment.
checkWithTerm :: String -> Core.Ty -> TypecheckM a -> TypecheckM a
checkWithTerm name ty = local (\env -> env{termEnv = (name, ty) : termEnv env})

withTypeVar :: String -> TypecheckM a -> TypecheckM a
withTypeVar name = local (\env -> env{typeVarNames = name : typeVarNames env})

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

lowerTyM :: AST.Ty -> TypecheckM Core.Ty
lowerTyM ty = case ty of
  AST.TInt -> pure Core.TInt
  AST.TFloat -> pure Core.TFloat
  AST.TBool -> pure Core.TBool
  AST.TString -> pure Core.TString
  AST.TUnit -> pure Core.TUnit
  AST.TFn arg ret -> Core.TFn <$> lowerTyM arg <*> lowerTyM ret
  AST.TCons name params -> Core.TCons name <$> mapM lowerTyM params
  AST.TVar name -> do
    env <- ask
    case name `elemIndex` typeVarNames env of
      Just idx -> pure $ Core.TVar idx
      Nothing -> throwError $ "Unbound type variable: " ++ name
  _ -> throwError $ "Cannot lower type: " ++ show ty

-- This is the core of System F.

----------------------------------------
-- Inference
infer :: Core.Expr -> TypecheckM Core.Ty
infer expr = case expr of
  Core.ELit lit -> inferLit lit
  Core.EUnit -> pure Core.TUnit
  Core.EVar idx -> lookupType idx
  Core.ELam paramTy body -> do
    bodyTy <- checkWithType paramTy (infer body)
    pure $ Core.TFn paramTy bodyTy

  -- Type abstraction: /\a -> body
  Core.EAbs body -> do
    bodyTy <- withTypeVar "a" (infer body)
    pure $ Core.TForall bodyTy
  -- Type application: e [T]
  Core.ETyApp e tyArg -> do
    eTy <- infer e
    case eTy of
      Core.TForall bodyTy -> pure $ tySubst 0 tyArg bodyTy
      _ -> throwError $ "Expected a polymorphic type got: " ++ show eTy
  Core.ELet ty val body -> do
    valTy <- infer val
    unless (valTy == ty) $
      throwError $
        "Type annotation mismatch: expected " ++ show ty ++ ", got " ++ show valTy
    checkWithType ty (infer body)
  _ -> undefined

inferLit :: AST.Literal -> TypecheckM Core.Ty
inferLit lit = case lit of
  AST.LitInt _ -> pure Core.TInt
  AST.LitFloat _ -> pure Core.TFloat
  AST.LitBool _ -> pure Core.TBool
  AST.LitString _ -> pure Core.TString

----------------------------------------
-- Checking

assertTy :: Core.Ty -> Core.Ty -> TypecheckM ()
assertTy expected actual =
  unless (expected == actual) $
    throwError $
      "Type mismatch: expected " ++ show expected ++ ", got " ++ show actual

check :: Core.Expr -> Core.Ty -> TypecheckM Core.Expr
check expr expectedTy = case expr of
  Core.ELit lit -> checkLit lit expectedTy
  Core.EUnit -> do
    assertTy expectedTy Core.TUnit
    pure Core.EUnit
  Core.EVar idx -> do
    ty <- lookupType idx
    assertTy expectedTy ty
    pure $ Core.EVar idx
  Core.ELam paramTy body -> case expectedTy of
    Core.TFn expectedParamTy expectedRetTy -> do
      assertTy expectedParamTy paramTy
      lamBody <- checkWithType paramTy (check body expectedRetTy)
      pure $ Core.ELam paramTy lamBody
    _ -> throwError "Expected function type / parameter count mismatch"
  _ -> undefined

checkLit :: AST.Literal -> Core.Ty -> TypecheckM Core.Expr
checkLit lit expectedTy = do
  litTy <- inferLit lit
  assertTy expectedTy litTy
  pure $ Core.ELit lit

checkPattern :: AST.Pat -> Core.Ty -> TypecheckM ()
checkPattern pat expectedTy = case pat of
  AST.PLit (AST.LitBool _) ->
    unless (expectedTy == Core.TBool) $
      throwError "Pattern type mismatch: expected Bool"
  AST.PLit (AST.LitInt _) ->
    unless (expectedTy == Core.TInt) $
      throwError "Pattern type mismatch: expected Int"
  _ -> throwError "Pattern checking not fully implemented"

-- checkLam :: Core.Expr -> Core.Ty -> TypecheckM Core.Expr
-- checkLam body ty = check body ty
-- checkLam body (Core.TFn paramTy retTy) = do
--   lamBody <- checkWithTerm paramName paramTy (checkLam ps body retTy)
--   pure $ Core.ELam paramTy lamBody
-- checkLam _ _ = throwError "Expected function type / parameter count mismatch"

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
