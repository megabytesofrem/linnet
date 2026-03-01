module Linnet.AST.Pattern
  ( Pat (..)
  )
where

import Data.List (intercalate)
import Linnet.AST.Types (Literal (..))
import Linnet.Prettyprint (Prettyprint (..), prettyPrintFoldable)

{-

These are patterns used in match expressions and function parameters

- Literals: 1, "hello", True
- Binders: x, y, z
- Constructor patterns: Just x, Nothing, Cons x xs
- Tuples: (x, y)
- Lists: [x, y, z]
- Partitions: (x:xs)
- Wildcards: _

-}

data Pat
  = PLit Literal
  | PVar String
  | PCons String [Pat] -- Constructor pattern, e.g. Just x, Cons x xs
  | PTuple [Pat] -- Tuple pattern, e.g. (x, y)
  | PList [Pat] -- List pattern, e.g. [x, y, z]
  | PPartition String Pat -- Partition pattern, e.g. (x:xs)
  | PWildcard -- Wildcard pattern, e.g. _
  deriving (Show, Eq)

-- Prettyprint

instance Prettyprint Pat where
  pretty (PLit lit) = pretty lit
  pretty (PVar name) = pure name
  pretty (PCons name pats) = do
    patStrs <- prettyPrintFoldable pats
    pure $ name <> " " <> unwords patStrs
  pretty (PTuple pats) = do
    patStrs <- prettyPrintFoldable pats
    pure $ "(" <> intercalate ", " patStrs <> ")"
  pretty (PList pats) = do
    patStrs <- prettyPrintFoldable pats
    pure $ "[" <> intercalate ", " patStrs <> "]"
  pretty (PPartition headPat tailPat) = do
    let headStr = headPat
    tailStr <- pretty tailPat
    pure $ headStr <> ":" <> tailStr
  pretty PWildcard = pure "_"