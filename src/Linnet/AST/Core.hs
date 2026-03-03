-- | AST for our "core" language.
module Linnet.AST.Core where

import Linnet.AST.Types qualified as AST

data Ty
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TVar Int -- Type variable, e.g. a
  | TFn Ty Ty -- Function type, e.g. Int -> Int
  | TForall Ty -- forall a. a -> a
  | TCons String [Ty] -- Type constructor with parameters
  deriving (Show, Eq)

data Expr
  = ELit AST.Literal
  | EUnit -- ()
  | EVar Int -- De Bruijn index for variables
  | ELam Ty Expr -- \x -> expr
  | EAbs Ty Expr -- /\a -> expr
  | EApp Expr Expr -- f x
  | ELet Ty Expr Expr -- let x : ty = expr1 in expr2
  deriving (Show, Eq)

-- After desugaring, we only keep top-level named definitions
data Def = Def
  { defName :: String
  , defType :: Ty
  , defBody :: Expr
  }
  deriving (Show, Eq)

-- Top-level program
newtype Program = Program [Def]
  deriving (Show, Eq)