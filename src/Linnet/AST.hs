module Linnet.AST
  ( Ty (..),
    Associativity (..),

    -- * AST
    UnaryOp (..),
    BinOp (..),
    Literal (..),
    Expr (..),

    -- * Binders
    Binder (..),
    LoopBinder (..),
  )
where

data Ty
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TFn Ty Ty -- Function type, e.g. Int -> Int
  | TVar String -- Type variable, e.g. a
  | TForall String Ty -- forall a. a -> a
  | TCons String [Ty] -- Type constructor with parameters
  deriving (Show, Eq)

-- The associativity of an operator, used in Parser.hs
data Associativity
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Show, Eq)

data UnaryOp
  = Negate -- -x
  deriving (Show, Eq)

{- ORMOLU_DISABLE -}
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | LtEq
  | GtEq
  -- * Functional operators
  | Apply   -- application: f x or f $ x
  | Compose -- composition: f . g

  | Bind    -- m >>= f
  | Pipe    -- x >> f
  | Map     -- f <$> x
  | UFO     -- f <*> x
  | Alt     -- x <|> y
  deriving (Show, Eq)
{- ORMOLU_ENABLE -}

data Literal
  = LitInt Integer
  | LitFloat Double
  | LitBool Bool
  | LitString String
  deriving (Show, Eq)

data Binder = Binder String (Maybe Ty)
  deriving (Show, Eq)

-- Binder used for loops, which can have a type annot and an initial value
data LoopBinder = LoopBinder String (Maybe Ty) Expr
  deriving (Show, Eq)

data Expr
  = ELit Literal
  | EUnit -- ()
  | EIdent String
  | ELam [String] Expr -- \x y -> expr
  | EUnaryOp UnaryOp Expr
  | EBinOp BinOp Expr Expr
  | EList [Expr] -- [1, 2, 3]
  | ETuple [Expr] -- (1, 2, 3)
  | EApp Expr Expr -- f x y z
  | ELet Binder Expr Expr -- let x = expr1 in expr2
  | EIf Expr Expr Expr -- if cond then expr1 else expr2
  | ELoop [LoopBinder] Expr -- loop (binders) expr
  deriving (Show, Eq)