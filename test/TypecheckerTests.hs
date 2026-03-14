module TypecheckerTests (typecheckerTests) where

import Linnet.AST qualified as AST
import Linnet.AST.Core qualified as Core
import Linnet.Typecheck.Typechecker
import Test.Tasty
import Test.Tasty.HUnit

-- | Shorter aliases
infers :: Core.Expr -> Either String Core.Ty
infers expr = runTypecheck defaultEnv (infer expr)

checks :: Core.Expr -> Core.Ty -> Either String Core.Expr
checks expr ty = runTypecheck defaultEnv (check expr ty)

infersWith :: Core.Expr -> Core.Ty -> Assertion
infersWith expr expected = infers expr @?= Right expected

infersFails :: Core.Expr -> Assertion
infersFails expr = case infers expr of
  Left _ -> pure ()
  Right ty -> assertFailure $ "Expected failure but got: " ++ show ty

checksWith :: Core.Expr -> Core.Ty -> Assertion
checksWith expr ty = case checks expr ty of
  Right _ -> pure ()
  Left err -> assertFailure $ "Expected success but got: " ++ err

checksFails :: Core.Expr -> Core.Ty -> Assertion
checksFails expr ty = case checks expr ty of
  Left _ -> pure ()
  Right _ -> assertFailure "Expected failure but got success"

-- | id = Λa. λx:a. x
idExpr :: Core.Expr
idExpr = Core.EAbs (Core.ELam (Core.TVar 0) (Core.EVar 0))

-- | const = Λa. Λb. λx:a. λy:b. x
constExpr :: Core.Expr
constExpr =
  Core.EAbs
    ( Core.EAbs
        ( Core.ELam
            (Core.TVar 1)
            (Core.ELam (Core.TVar 0) (Core.EVar 1))
        )
    )

typecheckerTests :: TestTree
typecheckerTests =
  testGroup
    "System F Tests"
    [ -- T-TAbs checks
      testCase "check id against ∀a. a -> a" $
        idExpr `checksWith` Core.TForall (Core.TFn (Core.TVar 0) (Core.TVar 0))
    , testCase "check id against wrong type fails" $
        checksFails idExpr (Core.TFn Core.TInt Core.TInt)
    , -- T-TAbs synths
      testCase "infer id: ∀a. a -> a" $
        idExpr `infersWith` Core.TForall (Core.TFn (Core.TVar 0) (Core.TVar 0))
    , -- T-TApp synths
      testCase "infer id @Int: Int -> Int" $
        Core.ETyApp idExpr Core.TInt
          `infersWith` Core.TFn Core.TInt Core.TInt
    , testCase "infer id @Bool: Bool -> Bool" $
        Core.ETyApp idExpr Core.TBool
          `infersWith` Core.TFn Core.TBool Core.TBool
    , testCase "type application on non-forall fails" $
        infersFails $
          Core.ETyApp (Core.ELam Core.TInt (Core.EVar 0)) Core.TInt
    , -- Full application
      testCase "infer id @Int 42: Int" $
        Core.EApp (Core.ETyApp idExpr Core.TInt) (Core.ELit (AST.LitInt 42))
          `infersWith` Core.TInt
    , testCase "infer id @Bool true: Bool" $
        Core.EApp (Core.ETyApp idExpr Core.TBool) (Core.ELit (AST.LitBool True))
          `infersWith` Core.TBool
    , testCase "id @Int applied to wrong type fails" $
        infersFails $
          Core.EApp (Core.ETyApp idExpr Core.TInt) (Core.ELit (AST.LitBool True))
    , -- Rank-2 / nested forall
      testCase "infer const: ∀a. ∀b. a -> b -> a" $
        constExpr
          `infersWith` Core.TForall
            ( Core.TForall
                (Core.TFn (Core.TVar 1) (Core.TFn (Core.TVar 0) (Core.TVar 1)))
            )
    , testCase "infer const @Int @Bool: Int -> Bool -> Int" $
        Core.ETyApp (Core.ETyApp constExpr Core.TInt) Core.TBool
          `infersWith` Core.TFn Core.TInt (Core.TFn Core.TBool Core.TInt)
    , testCase "infer const @Int @Bool 42: Bool -> Int" $
        Core.EApp
          (Core.ETyApp (Core.ETyApp constExpr Core.TInt) Core.TBool)
          (Core.ELit (AST.LitInt 42))
          `infersWith` Core.TFn Core.TBool Core.TInt
    ]