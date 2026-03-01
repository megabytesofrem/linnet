module Linnet.AST.Types
  ( Ty (..)
  , Literal (..)
  )
where

import Data.List (intercalate)
import Linnet.Prettyprint (Prettyprint (..), prettyPrintFoldable)

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

data Literal
  = LitInt Integer
  | LitFloat Double
  | LitBool Bool
  | LitString String
  deriving (Show, Eq)

-- Prettyprint

instance Prettyprint Ty where
  pretty TInt = pure "Int"
  pretty TFloat = pure "Float"
  pretty TBool = pure "Bool"
  pretty TString = pure "String"
  pretty TUnit = pure "()"
  pretty (TFn arg ret) = do
    argStr <- case arg of
      TFn _ _ -> (\s -> "(" <> s <> ")") <$> pretty arg
      _ -> pretty arg
    retStr <- pretty ret
    pure $ argStr <> " -> " <> retStr
  pretty (TVar name) = pure name
  pretty (TForall var ty) = do
    tyStr <- pretty ty
    pure $ "forall " <> var <> ". " <> tyStr
  pretty (TCons name params) = do
    paramStr <- prettyPrintFoldable params
    pure $ name <> "[" <> intercalate ", " paramStr <> "]"

instance Prettyprint Literal where
  pretty (LitInt n) = pure $ show n
  pretty (LitFloat f) = pure $ show f
  pretty (LitBool b) = pure $ show b
  pretty (LitString s) = pure $ show s