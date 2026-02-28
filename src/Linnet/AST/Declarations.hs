{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Linnet.AST.Declarations
  ( Ty (..)

    -- * AST
  , Literal (..)
  , Expr (..)
  , Decl (..)
  , TypeclassDeclaration (..)
  , TypeclassImplementation (..)

    -- * Binders
  , Binder (..)
  , LoopBinder (..)
  )
where

import Linnet.AST.Operators (BinOp, UnaryOp)

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

data Binder = Binder String (Maybe Ty)
  deriving (Show, Eq)

-- Binder used for loops, which can have a type annot and an initial value
data LoopBinder = LoopBinder String (Maybe Ty) Expr
  deriving (Show, Eq)

{- FOURMOLU_DISABLE -}
data Expr
  = ELit Literal
  | EUnit -- ()
  | EIdent String
  | EUnaryOp UnaryOp Expr
  | EBinOp BinOp Expr Expr
  | EList [Expr] -- [1, 2, 3]
  | ETuple [Expr] -- (1, 2, 3)
  | ELam [String] Expr -- \x y -> expr
  | EApp Expr Expr -- f x y z
  | ELet Binder Expr Expr -- let x = expr1 in expr2
  | EIf Expr Expr Expr -- if cond then expr1 else expr2
  | ELoop [LoopBinder] Expr -- loop (binders) expr
  
  -- Monadic binding and blocks
  | ELetM Binder Expr -- let! x = expr
  | EBind String Expr -- x <- m
  | EBlock [Expr]     -- do ... end blocks
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

type DataTypeConstructors = [(String, [Ty])]

data TypeclassDeclaration = TypeclassDecl
  { className :: String
  , typeParams :: [String]
  , methods :: [(String, [String], Ty)]
  }
  deriving (Show, Eq)

data TypeclassImplementation = TypeclassImpl
  { implClassName :: String
  , implType :: Ty
  , implMethods :: [(String, Expr)]
  }
  deriving (Show, Eq)

data Decl
  = ExprDeclaration Expr
  | DataDeclaration String [String] DataTypeConstructors
  | ClassDeclaration TypeclassDeclaration
  | ClassImplementation TypeclassImplementation
  deriving (Show, Eq)
