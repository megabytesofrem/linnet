module Linnet.Typecheck.Lower where

import Control.Monad (forM)
import Data.List (elemIndex)
import Linnet.AST.Core qualified as Core
import Linnet.AST.Declarations qualified as AST
import Linnet.AST.Operators (binOpToString, unOpToString)
import Linnet.AST.Pattern (Pat (..))

-- Context used during lowering to keep track of terms and types
data TypeContext = TypeContext
  { termEnv :: [String]
  , typeEnv :: [String]
  }
  deriving (Show, Eq)

-- | Create an empty typing context
mkContext :: TypeContext
mkContext = TypeContext{termEnv = [], typeEnv = []}

-- | Create a default typing context with built-in terms and types
defaultContext :: TypeContext
defaultContext =
  TypeContext
    { termEnv = ["Cons", "Nil", "True", "False"]
    , typeEnv = []
    }

try :: Maybe a -> String -> Either String a
try (Just x) _ = Right x
try Nothing err = Left err

lookupTerm :: TypeContext -> String -> Either String Int
lookupTerm ctx name = try (name `elemIndex` termEnv ctx) ("Unbound term variable: " ++ name)

lookupType :: TypeContext -> String -> Either String Int
lookupType ctx name = try (name `elemIndex` typeEnv ctx) ("Unbound type variable: " ++ name)

-- Lowering

-- Lower a type from the surface AST to the core AST
lowerTy :: TypeContext -> AST.Ty -> Either String Core.Ty
lowerTy _ctx AST.TInt = Right Core.TInt
lowerTy _ctx AST.TFloat = Right Core.TFloat
lowerTy _ctx AST.TBool = Right Core.TBool
lowerTy _ctx AST.TString = Right Core.TString
lowerTy _ctx AST.TUnit = Right Core.TUnit
lowerTy ctx (AST.TVar name) = Core.TVar <$> lookupType ctx name
lowerTy ctx (AST.TFn arg ret) = Core.TFn <$> lowerTy ctx arg <*> lowerTy ctx ret
lowerTy ctx (AST.TForall var ty) = Core.TForall <$> lowerTy newCtx ty
 where
  newCtx = ctx{typeEnv = var : typeEnv ctx}
lowerTy ctx (AST.TCons name params) = do
  paramTys <- traverse (lowerTy ctx) params
  Right $ Core.TCons name paramTys

-- Lower an expression from the surface AST to the core AST
lowerExpr :: TypeContext -> AST.Expr -> Either String Core.Expr
lowerExpr ctx expr = case expr of
  AST.ELit lit -> Right $ Core.ELit lit
  AST.EUnit -> Right Core.EUnit
  AST.EVar name -> Core.EVar <$> lookupTerm ctx name
  AST.EUnaryOp op e -> do
    -- Look up Debrujin index for the operator and apply it
    index <- lookupTerm ctx (unOpToString op)
    loweredExpr <- lowerExpr ctx e
    Right $ Core.EApp (Core.EVar index) loweredExpr
  AST.EBinOp op left right -> do
    loweredLeft <- lowerExpr ctx left
    loweredRight <- lowerExpr ctx right
    -- Look up Debrujin index for the operator and apply it
    index <- lookupTerm ctx (binOpToString op)
    Right $ Core.EApp (Core.EApp (Core.EVar index) loweredLeft) loweredRight
  AST.EList xs -> lowerList ctx xs
  AST.ETuple xs -> lowerTuple ctx xs
  --
  -- Catamorphism for lambda expressions: extend content
  AST.ELam params body -> do
    -- Extend context with parameter names
    let newCtx = ctx{termEnv = params ++ termEnv ctx}
    loweredBody <- lowerExpr newCtx body

    -- Lookup binder types from context to build the function type signature
    paramTys <- forM params $ \param -> case elemIndex param (termEnv newCtx) of
      Just idx -> Right $ Core.TVar idx
      Nothing -> Left $ "Unbound parameter in lambda: " ++ param

    -- Wrap the body in nested lambdas for each parameter
    Right $ foldr Core.ELam loweredBody paramTys
  -- Function application
  AST.EApp f x -> do
    loweredF <- lowerExpr ctx f
    loweredX <- lowerExpr ctx x
    Right $ Core.EApp loweredF loweredX
  AST.ELet (AST.Binder name mty) val body -> do
    loweredVal <- lowerExpr ctx val
    ty <- case mty of
      Just t -> lowerTy ctx t -- Lower type annotation if present
      Nothing -> Right Core.TUnit

    -- Extend context with new variable
    let newCtx = ctx{termEnv = name : termEnv ctx}
    loweredBody <- lowerExpr newCtx body
    Right $ Core.ELet ty loweredVal loweredBody
  AST.EIf cond thenBranch elseBranch -> do
    loweredCond <- lowerExpr ctx cond
    loweredThen <- lowerExpr ctx thenBranch
    loweredElse <- lowerExpr ctx elseBranch
    Right $
      Core.EMatch
        loweredCond
        [ (PLit (AST.LitBool True), loweredThen)
        , (PLit (AST.LitBool False), loweredElse)
        ]
  AST.EMatch scrutinee branches -> do
    loweredScrutinee <- lowerExpr ctx scrutinee
    loweredBranches <- traverse (lowerBranch ctx) branches
    Right $ Core.EMatch loweredScrutinee loweredBranches

  --
  _ -> Left "Lowering of expressions not implemented yet"

extractPatVars :: Pat -> [String]
extractPatVars (PLit _) = []
extractPatVars (PVar name) = [name]
extractPatVars (PCons _ pats) = concatMap extractPatVars pats
extractPatVars (PTuple pats) = concatMap extractPatVars pats
extractPatVars (PList pats) = concatMap extractPatVars pats
extractPatVars (PPartition hName tailPat) = extractPatVars (PVar hName) ++ extractPatVars tailPat
extractPatVars PWildcard = []

lowerBranch :: TypeContext -> (Pat, AST.Expr) -> Either String (Pat, Core.Expr)
lowerBranch ctx (pat, expr) = do
  -- Extend context with variables bound by the pattern
  let newCtx = ctx{termEnv = extractPatVars pat ++ termEnv ctx}

  loweredExpr <- lowerExpr newCtx expr
  Right (pat, loweredExpr)

lowerFunctionBody :: TypeContext -> AST.FunctionBody -> Either String Core.Expr
lowerFunctionBody ctx (AST.SimpleBody expr) = lowerExpr ctx expr
lowerFunctionBody ctx (AST.PatternBody branches) = do
  let scrutinee = Core.EVar 0 -- Placeholder for the scrutinee variable
  loweredBranches <- traverse (lowerBranch ctx) branches

  Right $ Core.EMatch scrutinee loweredBranches

-- Lower a top-level declaration from the surface AST to the core AST
lowerDecl :: TypeContext -> AST.Decl -> Either String [Core.Def]
lowerDecl ctx decl = case decl of
  AST.ExprDecl e -> do
    loweredExpr <- lowerExpr ctx e
    Right [Core.Def "_expr" Core.TUnit loweredExpr]
  AST.FunctionDecl (AST.FunctionDeclaration name params mty body) -> do
    -- Build the function type signature
    retTy <- case mty of
      Just t -> lowerTy ctx t
      Nothing -> Right Core.TUnit
    funcTy <- buildFunctionSignature ctx params retTy
    -- Extend context with parameter names for lowering the body
    let paramNames = map (\(AST.Binder n _) -> n) params
        newCtx = ctx{termEnv = paramNames ++ termEnv ctx}

    -- Lower the function body
    loweredBody <- lowerFunctionBody newCtx body

    Right [Core.Def name funcTy loweredBody]
  AST.DataDecl name params ctors -> do
    let newCtx = ctx{typeEnv = params ++ typeEnv ctx}
    traverse (desugarConstructor newCtx name params) ctors

  -- Implement when we add System F omega features
  AST.ClassDecl _ -> Left "Lowering of typeclass declarations not implemented yet"
  AST.ClassImpl _ -> Left "Lowering of typeclass implementations not implemented yet"
  -- Don't lower fixity declarations, discard them
  AST.FixityDecl{} -> Right []
 where
  -- TODO: Handle typeclass declarations and implementations
  -- _ -> Left "Lowering of this declaration type not implemented yet"

  desugarConstructor ctx' typeName typeParams (ctorName, ctorParamTys) = do
    ctorTy <- buildConstructorSignature ctx' typeName typeParams ctorParamTys

    -- Wrap in a forall (∀) for universal quantification over type parameters
    let wrappedTy = foldr (\_ acc -> Core.TForall acc) ctorTy typeParams
    Right $ Core.Def ctorName wrappedTy Core.EUnit

lowerProgram :: TypeContext -> AST.Program -> Either String Core.Program
lowerProgram ctx (AST.Program decls) =
  Core.Program . concat <$> traverse (lowerDecl ctx) decls

-- Build the type signature for a type constructor
-- typeName: Data type name ("List")
-- typeParams: Polymorphic type parameters (a)
-- paramTys: Types of the constructor parameters (e.g. Int for Cons)
--
-- Example: For "Cons : Int -> List a", typeName = "List", typeParams = ["a"], paramTys = [Int]
buildConstructorSignature :: TypeContext -> String -> [String] -> [AST.Ty] -> Either String Core.Ty
buildConstructorSignature ctx typeName typeParams paramTys = do
  desugaredParams <- traverse (lowerTy ctx) paramTys

  -- Build the return type (e.g. List a) using the type name and parameters
  let returnType = Core.TCons typeName (map Core.TVar [0 .. length typeParams - 1])

  -- Chain parameter types into a function type (e.g. Int -> List a)
  Right $ foldr Core.TFn returnType desugaredParams

-- Build the type signature for a function declaration
buildFunctionSignature :: TypeContext -> [AST.Binder] -> Core.Ty -> Either String Core.Ty
buildFunctionSignature ctx params retTy = go params
 where
  go [] = Right retTy
  go (AST.Binder _ mty : rest) = do
    -- Desugar the first parameter type, if any
    paramTy <- case mty of
      Just ty -> lowerTy ctx ty
      Nothing -> Right Core.TUnit

    -- Recursively build the remaining function type
    restTy <- go rest
    Right $ Core.TFn paramTy restTy

lowerList :: TypeContext -> [AST.Expr] -> Either String Core.Expr
lowerList ctx = expand
 where
  expand [] = do
    nilIndex <- lookupTerm ctx "Nil"
    Right $ Core.EVar nilIndex
  expand (x : xs) = do
    lowerHead <- lowerExpr ctx x
    lowerTail <- expand xs
    consIndex <- lookupTerm ctx "Cons"
    Right $ Core.EApp (Core.EApp (Core.EVar consIndex) lowerHead) lowerTail

lowerTuple :: TypeContext -> [AST.Expr] -> Either String Core.Expr
lowerTuple = lowerList
