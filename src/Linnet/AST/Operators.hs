module Linnet.AST.Operators
  ( UnaryOp (..)
  , BinOp (..)

    -- * Operator associativity
  , Associativity (..)
  )
where

import Linnet.Prettyprint (Prettyprint (..))

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

-- Prettyprint

instance Prettyprint UnaryOp where
  pretty Negate = pure "-"

instance Prettyprint BinOp where
  pretty Add = pure "+"
  pretty Sub = pure "-"
  pretty Mul = pure "*"
  pretty Div = pure "/"
  pretty Mod = pure "%"
  pretty Eq = pure "=="
  pretty Neq = pure "/="
  pretty Lt = pure "<"
  pretty Gt = pure ">"
  pretty LtEq = pure "<="
  pretty GtEq = pure ">="
  -- Functional operators
  pretty Apply = pure " "
  pretty Compose = pure "."
  pretty Bind = pure ">>="
  pretty Pipe = pure ">>"
  pretty Map = pure "<$>"
  pretty UFO = pure "<*>"
  pretty Alt = pure "<|>"