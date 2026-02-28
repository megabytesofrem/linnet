module Linnet.AST.Operators
  ( UnaryOp (..)
  , BinOp (..)

    -- * Operator associativity
  , Associativity (..)
  )
where

-- The associativity of an operator, used in Parser.hs
data Associativity
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Show, Eq)

data UnaryOp
  = Negate -- -x
  deriving (Show, Eq)

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}