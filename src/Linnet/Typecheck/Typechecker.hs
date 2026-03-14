{- HLINT ignore "Avoid lambda" -}

module Linnet.Typecheck.Typechecker where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (elemIndex, findIndex)
import Linnet.AST qualified as AST
import Linnet.AST.Core qualified as Core
import Linnet.Typecheck.Monad (TypecheckM, runTypecheckM)

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

-- Typechecker monad parameterized by the typing environment
type TypecheckerM a = TypecheckM TypecheckEnv a

data TypecheckEnv = TypecheckEnv
  { typeEnv :: [Core.Ty] -- Type environment for type variables
  , typeVarNames :: [String]
  , termEnv :: [(String, Core.Ty)] -- Term environment for variable names and their types
  }
  deriving (Show, Eq)

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
checkWithType :: Core.Ty -> TypecheckerM a -> TypecheckerM a
checkWithType ty = local (\env -> env{typeEnv = ty : typeEnv env})

-- Extend the term environment with a new variable and run a new typechecking action
-- in that environment.
checkWithTerm :: String -> Core.Ty -> TypecheckerM a -> TypecheckerM a
checkWithTerm name ty = local (\env -> env{termEnv = (name, ty) : termEnv env})

withTypeVar :: String -> TypecheckerM a -> TypecheckerM a
withTypeVar name = local (\env -> env{typeVarNames = name : typeVarNames env})

-- Lookup a type variable in the environment
lookupType :: Int -> TypecheckerM Core.Ty
lookupType idx = do
  env <- ask
  if idx < length (typeEnv env)
    then pure $ typeEnv env !! idx
    else throwError $ "Unbound type variable: " ++ show idx

lookupTerm :: String -> TypecheckerM (Int, Core.Ty)
lookupTerm name = do
  env <- ask
  case findIndex (\(n, _) -> n == name) (termEnv env) of
    -- Make this not partial
    Just idx -> pure (idx, snd $ termEnv env !! idx)
    Nothing -> throwError $ "Unbound term variable: " ++ name

lookupTermBrujin :: Int -> TypecheckerM (String, Core.Ty)
lookupTermBrujin idx = do
  env <- ask
  if idx < length (termEnv env)
    then pure $ termEnv env !! idx
    else throwError $ "Unbound term variable: " ++ show idx

lowerTyM :: AST.Ty -> TypecheckerM Core.Ty
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
infer :: Core.Expr -> TypecheckerM Core.Ty
infer expr = case expr of
  Core.ELit lit -> inferLit lit
  Core.EUnit -> pure Core.TUnit
  Core.EVar idx -> do
    env <- ask
    if idx < length (termEnv env)
      then pure . snd $ termEnv env !! idx
      else throwError $ "Unbound variable index: " ++ show idx
  Core.ELam paramTy body -> do
    -- Extend the term environment with a new variable for the parameter and infer the body type
    bodyTy <- checkWithTerm "_lam" paramTy (infer body)
    pure $ Core.TFn paramTy bodyTy

  -- Type abstraction: /\a -> body, introduces a type variable binder
  Core.EAbs body -> do
    -- Extend the type environment with a new type variable and infer the body type
    bodyTy <-
      withTypeVar "a" (infer body)

    pure $ Core.TForall bodyTy
  -- Type application: e [T], substitutes a type argument into a polymorphic function
  Core.ETyApp e tyArg -> do
    eTy <- infer e
    case eTy of
      Core.TForall bodyTy -> pure $ tySubst 0 tyArg bodyTy
      _ -> throwError $ "Expected a polymorphic type, got: " ++ show eTy
  -- Term application: e₁ e₂
  Core.EApp func arg -> do
    funcTy <- infer func
    case funcTy of
      Core.TFn paramTy retTy -> do
        -- Check the argument against the parameter type
        _ <- check arg paramTy
        pure retTy
      _ -> throwError $ "Expected a function type, got: " ++ show funcTy
  Core.ELet ty val body -> do
    valTy <- infer val
    ty =:= valTy
    checkWithTerm "_let" ty (infer body)
  Core.EMatch e branches -> do
    eTy <- infer e
    branchTys <- forM branches $ \(pat, branchExpr) -> do
      checkPat pat eTy
      infer branchExpr

    -- Ensure all branches have the same type
    case branchTys of
      [] -> throwError "Empty match expression"
      (first : rest) -> do
        forM_ rest $ \ty ->
          unless (ty == first) $
            throwError $
              "Branch type mismatch: expected " ++ show first ++ ", got " ++ show ty
        pure first

inferLit :: AST.Literal -> TypecheckerM Core.Ty
inferLit lit = case lit of
  AST.LitInt _ -> pure Core.TInt
  AST.LitFloat _ -> pure Core.TFloat
  AST.LitBool _ -> pure Core.TBool
  AST.LitString _ -> pure Core.TString

----------------------------------------
-- Checking

assertTy :: Core.Ty -> Core.Ty -> TypecheckerM ()
assertTy expected actual =
  unless (expected == actual) $
    throwError $
      "Type mismatch: expected " ++ show expected ++ ", got " ++ show actual

-- Infix operator for type assertion

infix 4 =:=

(=:=) :: Core.Ty -> Core.Ty -> TypecheckerM ()
(=:=) = assertTy

check :: Core.Expr -> Core.Ty -> TypecheckerM Core.Expr
check expr expectedTy = case expr of
  Core.ELit lit -> checkLit lit expectedTy
  Core.EUnit -> do
    expectedTy =:= Core.TUnit
    pure Core.EUnit
  Core.EVar idx -> do
    -- Lookup variable type from term environment and assert it matches the expected type
    (_name, varTy) <- lookupTermBrujin idx
    varTy =:= expectedTy
    pure $ Core.EVar idx
  Core.ELam paramTy body -> case expectedTy of
    Core.TFn expectedParamTy expectedRetTy -> do
      paramTy =:= expectedParamTy
      lamBody <- checkWithTerm "_lam" paramTy (check body expectedRetTy)
      pure $ Core.ELam paramTy lamBody
    _ -> throwError "Expected function type / parameter count mismatch"
  -- Type abstraction: /\a -> body, introduces a type variable binder
  Core.EAbs body -> case expectedTy of
    Core.TForall expectedBodyTy -> do
      body' <- withTypeVar "a" (check body expectedBodyTy)
      pure $ Core.EAbs body'
    _ -> throwError "Expected polymorphic type for type abstraction"
  -- Type application: e [T], substitutes a type argument into a polymorphic function
  Core.ETyApp e tyArg -> do
    eTy <- infer e
    case eTy of
      Core.TForall bodyTy -> do
        let instantiatedTy = tySubst 0 tyArg bodyTy
        instantiatedTy =:= expectedTy
        pure $ Core.ETyApp e tyArg
      _ -> throwError $ "Expected a polymorphic type, got: " ++ show e
  Core.EApp func arg -> do
    funcTy <- infer func
    case funcTy of
      Core.TFn paramTy retTy -> do
        retTy =:= expectedTy

        -- Check the argument against the parameter type
        arg' <- check arg paramTy
        pure $ Core.EApp func arg'
      _ -> throwError $ "Expected a function type, got: " ++ show funcTy
  --
  -- Subsumption: synthesize and compare (fallthrough case)
  _ -> do
    inferredTy <- infer expr
    inferredTy =:= expectedTy
    pure expr

checkLit :: AST.Literal -> Core.Ty -> TypecheckerM Core.Expr
checkLit lit expectedTy = do
  litTy <- inferLit lit
  expectedTy =:= litTy
  pure $ Core.ELit lit

checkPat :: AST.Pat -> Core.Ty -> TypecheckerM ()
checkPat pat expectedTy = case pat of
  AST.PLit (AST.LitBool _) ->
    unless (expectedTy == Core.TBool) $
      throwError "Pattern type mismatch: expected Bool"
  AST.PLit (AST.LitInt _) ->
    unless (expectedTy == Core.TInt) $
      throwError "Pattern type mismatch: expected Int"
  _ -> throwError "Pattern checking not fully implemented"

buildList :: [Core.Expr] -> TypecheckerM Core.Expr
buildList exprs = do
  (consIdx, _) <- lookupTerm "Cons"
  (nilIdx, _) <- lookupTerm "Nil"
  pure $
    foldr
      (\e acc -> Core.EApp (Core.EApp (Core.EVar consIdx) e) acc)
      (Core.EVar nilIdx)
      exprs

buildTuple :: [Core.Expr] -> TypecheckerM Core.Expr
buildTuple [] = pure Core.EUnit
buildTuple [e] = pure e
buildTuple (e : es) = do
  tupleRest <- buildTuple es
  (tupleIdx, _) <- lookupTerm "Tuple"
  pure $ Core.EApp (Core.EApp (Core.EVar tupleIdx) e) tupleRest

-- | Run the typechecker with a given type environment
runTypecheck :: TypecheckEnv -> TypecheckerM a -> Either String a
runTypecheck env m = runExcept (runReaderT (runTypecheckM m) env)
