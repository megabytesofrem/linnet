module Linnet.Parse.Types
  ( pType
  , pAtomTy
  , pAppTy
  , pArrowTy
  , pForAll
  )
where

import Control.Monad (guard)
import Linnet.AST (Ty (..))
import Linnet.Parse.Lexer
import Text.Megaparsec

-- * Type parsers

pType :: Parser Ty
pType = try pForAll <|> pArrowTy

-- An atomic type: either a primitive type, a type variable, or a parenthesized type
pAtomTy :: Parser Ty
pAtomTy =
  choice
    [ TInt <$ symbol "Int"
    , TFloat <$ symbol "Float"
    , TBool <$ symbol "Bool"
    , TString <$ symbol "String"
    , TUnit <$ symbol "()"
    , TVar <$> pIdent
    , parens pType
    ]
 where
  parens = enclosed '(' ')'

-- Type application: Maybe Int, List String, etc.
-- Type constructor applied to zero or more type arguments (atoms)
pAppTy :: Parser Ty
pAppTy =
  choice
    [ try $ do
        ctor <- pCtorIdent
        guard (not $ isPrimitive ctor) -- Don't allow primitive types as constructors
        params <- many pAppTy
        pure $ TCons ctor params
    , pAtomTy
    ]

-- Arrow types: Int -> Int, Maybe Int -> Bool, etc.
pArrowTy :: Parser Ty
pArrowTy = do
  arg <- pAppTy
  rest <- optional (altSym "->" "→" >> pArrowTy)
  pure $ case rest of
    Just ret -> TFn arg ret
    Nothing -> arg

-- Forall types: forall a. a -> a, etc.
pForAll :: Parser Ty
pForAll = do
  _ <- symbol "∀" <|> symbol "forall"
  tyvars <- some pIdent
  _ <- symbol "."
  ty <- pArrowTy
  pure $ foldr TForall ty tyvars