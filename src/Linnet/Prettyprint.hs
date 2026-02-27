module Linnet.Prettyprint where

import Data.List (foldl', intercalate)
import Linnet.AST

-- State for the pretty printer
newtype PrettyprintState = PrettyprintState
  { indentLevel :: Int
  }

indentsToSpaces :: PrettyprintState -> String
indentsToSpaces p = replicate (indentLevel p * 2) ' '

-- Helper function to pretty print any Foldable collection of Prettyprint items
-- We thread the state through to maintain correct indentation if needed
prettyPrintFoldable :: (Prettyprint s, Foldable t) => PrettyprintState -> t s -> ([String], PrettyprintState)
prettyPrintFoldable initialP =
  foldl'
    ( \(acc, st) el ->
        let (str, st') = pretty st el in (acc ++ [str], st')
    )
    ([], initialP)

-- Typeclass for pretty printing AST nodes
class Prettyprint a where
  -- | `pretty` takes a PrettyprintState to keep track of indent level
  pretty :: PrettyprintState -> a -> (String, PrettyprintState)

instance Prettyprint Ty where
  pretty p TInt = ("Int", p)
  pretty p TFloat = ("Float", p)
  pretty p TBool = ("Bool", p)
  pretty p TString = ("String", p)
  pretty p TUnit = ("()", p)
  pretty p (TFn arg ret) =
    let argStr = case arg of
          TFn _ _ -> "(" ++ fst (pretty p arg) ++ ")"
          _ -> fst (pretty p arg)
        (retStr, p') = pretty p ret
     in (argStr ++ " -> " ++ retStr, p')
  pretty p (TVar name) = (name, p)
  pretty p (TForall var ty) =
    let (tyStr, p') = pretty p ty
     in ("forall " ++ var ++ ". " ++ tyStr, p')
  pretty p (TCons name params) =
    let (paramsStr, p') = prettyPrintFoldable p params
     in (name ++ "[" ++ intercalate ", " paramsStr ++ "]", p')

instance Prettyprint UnaryOp where
  pretty p Negate = ("-", p)

instance Prettyprint BinOp where
  pretty p Add = ("+", p)
  pretty p Sub = ("-", p)
  pretty p Mul = ("*", p)
  pretty p Div = ("/", p)
  pretty p Mod = ("%", p)
  pretty p Eq = ("==", p)
  pretty p Neq = ("/=", p)
  pretty p Lt = ("<", p)
  pretty p Gt = (">", p)
  pretty p LtEq = ("<=", p)
  pretty p GtEq = (">=", p)
  -- \* Functional operators
  pretty p Apply = (" ", p)
  pretty p Compose = (".", p)
  pretty p Bind = (">>=", p)
  pretty p Pipe = (">>", p)
  pretty p Map = ("<$>", p)
  pretty p UFO = ("<*>", p)
  pretty p Alt = ("<|>", p)

instance Prettyprint Literal where
  pretty p (LitInt n) = (show n, p)
  pretty p (LitFloat f) = (show f, p)
  pretty p (LitBool b) = (show b, p)
  pretty p (LitString s) = (show s, p)

instance Prettyprint Binder where
  pretty p (Binder name mty) =
    let tyStr = case mty of
          Just ty -> ": " ++ fst (pretty p ty)
          Nothing -> ""
     in (name ++ tyStr, p)

instance Prettyprint LoopBinder where
  pretty p (LoopBinder name mty initVal) =
    let tyStr = case mty of
          Just ty -> ": " ++ fst (pretty p ty)
          Nothing -> ""
        initStr = " = " ++ fst (pretty p initVal)
     in (name ++ tyStr ++ initStr, p)

instance Prettyprint Expr where
  pretty p (ELit lit) = pretty p lit
  pretty p EUnit = ("()", p)
  pretty p (EIdent name) = (name, p)
  pretty p (EUnaryOp op expr) =
    let (opStr, p') = pretty p op
        (exprStr, p'') = pretty p' expr
     in (opStr ++ exprStr, p'')
  pretty p (EBinOp op left right) =
    let (leftStr, p') = pretty p left
        (opStr, p'') = pretty p' op
        (rightStr, p''') = pretty p'' right
     in (leftStr ++ " " ++ opStr ++ " " ++ rightStr, p''')
  pretty p (EList elems) =
    let (elemsStr, p') = prettyPrintFoldable p elems
     in ("[" ++ intercalate ", " elemsStr ++ "]", p')
  pretty p (ETuple elems) =
    let (elemsStr, p') = prettyPrintFoldable p elems
     in ("(" ++ intercalate ", " elemsStr ++ ")", p')
  pretty p (ELam params body) =
    let (bodyStr, p') = pretty p body
     in ("\\" ++ unwords params ++ " -> " ++ bodyStr, p')
  pretty p (EApp func arg) =
    let (funcStr, p') = pretty p func
        (argStr, p'') = pretty p' arg
     in (funcStr ++ " " ++ argStr, p'')
  pretty p (ELet binder val body) =
    let (binderStr, p') = pretty p binder
        (valStr, p'') = pretty p' val
        (bodyStr, p''') = pretty p'' body
     in ("let " ++ binderStr ++ " = " ++ valStr ++ " in " ++ bodyStr, p''')
  pretty p (EIf cond thenBranch elseBranch) =
    let (condStr, p') = pretty p cond
        (thenStr, p'') = pretty p' thenBranch
        (elseStr, p''') = pretty p'' elseBranch
     in ("if " ++ condStr ++ " then " ++ thenStr ++ " else " ++ elseStr, p''')
  pretty p (ELoop binders body) =
    let (bindersStr, p') = prettyPrintFoldable p binders
        (bodyStr, p'') = pretty p' body
     in ("loop { " ++ intercalate "; " bindersStr ++ " } " ++ bodyStr, p'')
  pretty p _ = ("<expr>", p) -- Fallback for unhandled cases

prettyPrint :: (Prettyprint p) => PrettyprintState -> p -> (String, PrettyprintState)
prettyPrint = pretty