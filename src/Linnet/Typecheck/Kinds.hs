-- Kind checking pass for Linnets type system
--
-- Kinds allow us to abstract over type constructors. For example, `List` has kind `* → *`
-- because it takes a type (like `Int`) and produces a new type (`List Int`).
module Linnet.Typecheck.Kinds where

import Control.Monad (forM_)
import Linnet.AST.Core (Kind (..))
import Linnet.AST.Core qualified as Core

type KindEnv = [Kind]

-- Lookup the kind of a type variable in the environment
lookupKind :: KindEnv -> Int -> Either String Kind
lookupKind env idx
  | idx < length env = Right (env !! idx)
  | otherwise = Left ("Unbound type variable: " ++ show idx)

-- Check that a list of types all have kind *
allStars :: KindEnv -> [Core.Ty] -> Either String ()
allStars env tys = forM_ tys $ \ty -> do
  k <- inferKind env ty
  if k == Star
    then Right ()
    else Left "Expected kind *"

inferKind :: KindEnv -> Core.Ty -> Either String Kind
inferKind _ Core.TInt = Right Star
inferKind _ Core.TFloat = Right Star
inferKind _ Core.TBool = Right Star
inferKind _ Core.TString = Right Star
inferKind _ Core.TUnit = Right Star
inferKind env (Core.TVar idx _) = lookupKind env idx
inferKind env (Core.TFn arg ret) = do
  kArg <- inferKind env arg
  kRet <- inferKind env ret
  env `allStars` [arg, ret]
  Right Star
inferKind env (Core.TForall ty) = do
  -- Extend the environment with a new type variable of kind *
  _ <- inferKind (Star : env) ty
  Right Star
inferKind env (Core.TCons _ params k) = do
  -- Check that all parameters have kind *
  env `allStars` params
  Right k

--
