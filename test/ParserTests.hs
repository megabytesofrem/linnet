module ParserTests (parserTests) where

import Linnet.AST
import Linnet.Parser (pExpr, pLiteral, pType)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (Parsec, Stream, parseMaybe)

-- Shorter alias for parsing in tests
parses :: (Ord e, Stream s) => Parsec e s a -> s -> Maybe a
parses = parseMaybe

literalTests :: TestTree
literalTests =
  testGroup
    "Literal Tests"
    [ testCase "parse integer literal" $
        parses pLiteral "42" @?= Just (LitInt 42)
    , -- FIX: Parsing floats
      -- testCase "parse float literal" $
      --   parses pLiteral "3.14" @?= Just (LitFloat 3.14),
      testCase "parse boolean literal" $
        parses pLiteral "true" @?= Just (LitBool True)
    , testCase "parse string literal" $
        parses pLiteral "\"hello\"" @?= Just (LitString "hello")
    ]

exprTests :: TestTree
exprTests =
  testGroup
    "Expression Tests"
    [ testCase "parse identifier" $
        parses pExpr "x" @?= Just (EIdent "x")
    , testCase "parse unit: ()" $
        parses pExpr "()" @?= Just EUnit
    , testCase "parse simple addition" $
        parses pExpr "1 + 2"
          @?= Just (EBinOp Add (ELit (LitInt 1)) (ELit (LitInt 2)))
    , testCase "parse nested expressions" $
        parses pExpr "1 + 2 * 3"
          @?= Just (EBinOp Add (ELit (LitInt 1)) (EBinOp Mul (ELit (LitInt 2)) (ELit (LitInt 3))))
    , testCase "parse parentheses" $
        parses pExpr "(1 + 2) * 3"
          @?= Just (EBinOp Mul (EBinOp Add (ELit (LitInt 1)) (ELit (LitInt 2))) (ELit (LitInt 3)))
    , testCase "parse list expression: [1, 2, 3]" $
        parses pExpr "[1, 2, 3]"
          @?= Just (EList [ELit (LitInt 1), ELit (LitInt 2), ELit (LitInt 3)])
    , testCase "parse tuple expression: (1, 2, 3)" $
        parses pExpr "(1, 2, 3)"
          @?= Just (ETuple [ELit (LitInt 1), ELit (LitInt 2), ELit (LitInt 3)])
    , testCase "parse lambda expression: \\x -> x" $
        parses pExpr "\\x -> x"
          @?= Just (ELam ["x"] (EIdent "x"))
    , testCase "parse lambda expression: \\x y -> x + y" $
        parses pExpr "\\x y -> x + y"
          @?= Just (ELam ["x", "y"] (EBinOp Add (EIdent "x") (EIdent "y")))
    , testCase "parse let expression: let x = 1 in x + 2" $
        parses pExpr "let x = 1 in x + 2"
          @?= Just (ELet (Binder "x" Nothing) (ELit (LitInt 1)) (EBinOp Add (EIdent "x") (ELit (LitInt 2))))
    , testCase "parse if expression: if x then y else z" $
        parses pExpr "if x then y else z"
          @?= Just (EIf (EIdent "x") (EIdent "y") (EIdent "z"))
    ]

typeTests :: TestTree
typeTests =
  testGroup
    "Type Tests"
    [ testCase "parse simple type: Int" $
        parses pType "Int" @?= Just TInt
    , testCase "parse type variable: a" $
        parses pType "a" @?= Just (TVar "a")
    , testCase "parse type constructor: Maybe[Int]" $
        parses pType "Maybe[Int]" @?= Just (TCons "Maybe" [TInt])
    , testCase "parse type constructor: Maybe[a]" $
        parses pType "Maybe[a]" @?= Just (TCons "Maybe" [TVar "a"])
    , testCase "parse arrow type: Int -> Int" $
        parses pType "Int -> Int" @?= Just (TFn TInt TInt)
    , testCase "parse arrow type: a -> b" $
        parses pType "a -> b" @?= Just (TFn (TVar "a") (TVar "b"))
    , testCase "parse arrow type: (Int -> Int) -> Int" $
        parses pType "(Int -> Int) -> Int" @?= Just (TFn (TFn TInt TInt) TInt)
    ]

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
    [ literalTests
    , exprTests
    , typeTests
    ]