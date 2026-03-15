-- | AST for our "core" language.
module Linnet.AST.Core where

import Linnet.AST (Pat)
import Linnet.AST.Types qualified as AST

data Ty
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TVar Int Kind -- Type variable, e.g. a : *, f : * -> *
  | TFn Ty Ty -- Function type, e.g. Int -> Int
  | TForall Ty -- forall a. a -> a
  | TCons String [Ty] Kind -- Maybe : * -> *, List : * -> *, etc.
  deriving (Show, Eq)

data Kind
  = Star -- The kind of all proper types.
  | KArrow Kind Kind -- Kind arrow, e.g. * -> *
  deriving (Eq, Show)

data Expr
  = ELit AST.Literal
  | EUnit -- ()
  | EVar Int -- De Bruijn index for variables
  | ELam Ty Expr -- \x -> expr
  | EAbs Expr -- /\a -> expr    (type abstraction)
  | ETyApp Expr Ty -- e [Int]   (type application)
  | EApp Expr Expr -- e₁ e₂      (term application)
  | ELet Ty Expr Expr -- let x : ty = expr1 in expr2
  | EMatch Expr [(Pat, Expr)] -- match expr with | pat -> expr
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