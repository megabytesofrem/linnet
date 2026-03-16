module ParserTests () where

import Linnet.AST

import Data.Void (Void)
import Linnet.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (Parsec, Stream, parseMaybe)

-- Shorter alias for parsing in tests
parses :: (Ord e, Stream s) => Parsec e s a -> s -> Maybe a
parses = parseMaybe

parseDecl' :: Parsec Void String Decl
parseDecl' = parseDecl defaultFixityEnv

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
        parses parseExpr "x" @?= Just (EVar "x")
    , testCase "parse unit: ()" $
        parses parseExpr "()" @?= Just EUnit
    , testCase "parse simple addition" $
        parses parseExpr "1 + 2"
          @?= Just (EBinOp Add (ELit (LitInt 1)) (ELit (LitInt 2)))
    , testCase "parse nested expressions" $
        parses parseExpr "1 + 2 * 3"
          @?= Just (EBinOp Add (ELit (LitInt 1)) (EBinOp Mul (ELit (LitInt 2)) (ELit (LitInt 3))))
    , testCase "parse parentheses" $
        parses parseExpr "(1 + 2) * 3"
          @?= Just (EBinOp Mul (EBinOp Add (ELit (LitInt 1)) (ELit (LitInt 2))) (ELit (LitInt 3)))
    , testCase "parse list expression: [1, 2, 3]" $
        parses parseExpr "[1, 2, 3]"
          @?= Just (EList [ELit (LitInt 1), ELit (LitInt 2), ELit (LitInt 3)])
    , testCase "parse tuple expression: (1, 2, 3)" $
        parses parseExpr "(1, 2, 3)"
          @?= Just (ETuple [ELit (LitInt 1), ELit (LitInt 2), ELit (LitInt 3)])
    , testCase "parse lambda expression: \\x -> x" $
        parses parseExpr "\\x -> x"
          @?= Just (ELam ["x"] (EVar "x"))
    , testCase "parse lambda expression: \\x y -> x + y" $
        parses parseExpr "\\x y -> x + y"
          @?= Just (ELam ["x", "y"] (EBinOp Add (EVar "x") (EVar "y")))
    , testCase "parse let expression: let x = 1 in x + 2" $
        parses parseExpr "let x = 1 in x + 2"
          @?= Just (ELet (Binder "x" Nothing) (ELit (LitInt 1)) (EBinOp Add (EVar "x") (ELit (LitInt 2))))
    , testCase "parse if expression: if x then y else z" $
        parses parseExpr "if x then y else z"
          @?= Just (EIf (EVar "x") (EVar "y") (EVar "z"))
    ]

typeTests :: TestTree
typeTests =
  testGroup
    "Type Tests"
    [ testCase "parse simple type: Int" $
        parses pType "Int" @?= Just TInt
    , testCase "parse type variable: a" $
        parses pType "a" @?= Just (TVar "a")
    , testCase "parse type constructor: Maybe Int" $
        parses pType "Maybe Int" @?= Just (TCons "Maybe" [TInt])
    , testCase "parse type constructor: Maybe a" $
        parses pType "Maybe a" @?= Just (TCons "Maybe" [TVar "a"])
    , testCase "parse type constructor: Either String Int" $
        parses pType "Either String Int" @?= Just (TCons "Either" [TString, TInt])
    , testCase "parse arrow type: Int -> Int" $
        parses pType "Int -> Int" @?= Just (TFn TInt TInt)
    , testCase "parse arrow type: a -> b" $
        parses pType "a -> b" @?= Just (TFn (TVar "a") (TVar "b"))
    , testCase "parse arrow type: (Int -> Int) -> Int" $
        parses pType "(Int -> Int) -> Int" @?= Just (TFn (TFn TInt TInt) TInt)
    ]

-- declTests :: TestTree
-- declTests =
--   testGroup
--     "Declaration Tests"
--     [ testCase "parse expression declaration: 1 + 2" $
--         parses parseDecl' "1 + 2" @?= Just (ExprDecl (EBinOp Add (ELit (LitInt 1)) (ELit (LitInt 2))))
--     , testCase "parse function declaration: name : a -> a; def name x = x" $
--         parses parseDecl' "name : a -> a\ndef name x = x"
--           @?= Just (FunctionDecl (FunctionDeclaration "name" [Binder "x" Nothing] (Just (TFn (TVar "a") (TVar "a"))) (SimpleBody (EVar "x"))))
--     ]

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
    [ literalTests
    , exprTests
    , typeTests
    -- , declTests
    ]