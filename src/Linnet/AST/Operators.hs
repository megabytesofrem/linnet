module Linnet.AST.Operators
  ( UnaryOp (..)
  , BinOp (..)

    -- * Operator associativity
  , Associativity (..)
  , Fixity (..)
  , FixityEnv
  , builtinOps
  , defaultFixityEnv
  , unOpToString
  , binOpToString
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Linnet.Prettyprint (Prettyprint (..))

-- The associativity of an operator, used in Parser.hs
data Associativity
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Show, Eq)

data Fixity = Fixity
  { fixityAssoc :: Associativity
  , fixityPrec :: Int
  }
  deriving (Show, Eq)

-- | Environment mapping operator names to their fixity
type FixityEnv = Map String Fixity

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
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

builtinOps :: Map String BinOp
builtinOps =
  M.fromList
    [ ("+", Add)
    , ("-", Sub)
    , ("*", Mul)
    , ("/", Div)
    , ("%", Mod)
    , ("==", Eq)
    , ("/=", Neq)
    , -- Mathematical alternative syntax
      ("≡", Eq)
    , ("≠", Neq)
    , ("≤", LtEq)
    , ("≥", GtEq)
    , --
      ("<", Lt)
    , (">", Gt)
    , ("<=", LtEq)
    , (">=", GtEq)
    , (".", Compose)
    , ("∘", Compose)
    , ("$", Apply)
    ]

defaultFixityEnv :: FixityEnv
defaultFixityEnv =
  M.fromList
    [ ("*", Fixity AssocLeft 7)
    , ("/", Fixity AssocLeft 7)
    , ("%", Fixity AssocLeft 7)
    , ("+", Fixity AssocLeft 6)
    , ("-", Fixity AssocLeft 6)
    , ("==", Fixity AssocNone 4)
    , ("/=", Fixity AssocNone 4)
    , ("<", Fixity AssocNone 4)
    , (">", Fixity AssocNone 4)
    , ("<=", Fixity AssocNone 4)
    , (">=", Fixity AssocNone 4)
    , -- Mathematical alternative syntax
      ("≡", Fixity AssocNone 4)
    , ("≠", Fixity AssocNone 4)
    , ("≤", Fixity AssocNone 4)
    , ("≥", Fixity AssocNone 4)
    , -- Functional operators
      (".", Fixity AssocRight 9)
    , ("∘", Fixity AssocRight 9)
    , ("$", Fixity AssocRight 0)
    , (">>=", Fixity AssocLeft 1)
    , (">>", Fixity AssocLeft 1)
    , ("<$>", Fixity AssocLeft 4)
    , ("<*>", Fixity AssocLeft 4)
    , ("<|>", Fixity AssocLeft 3)
    ]

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

unOpToString :: UnaryOp -> String
unOpToString op = case op of
  Negate -> "-"

binOpToString :: BinOp -> String
binOpToString op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Eq -> "=="
  Neq -> "/="
  Lt -> "<"
  Gt -> ">"
  LtEq -> "<="
  GtEq -> ">="
  Apply -> " "
  Compose -> "."