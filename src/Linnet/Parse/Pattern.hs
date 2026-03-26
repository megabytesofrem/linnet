module Linnet.Parse.Pattern
  ( pPattern
  )
where

import Linnet.AST
import Linnet.Parse.Lexer
import Text.Megaparsec

-----------------------------------------
-- Pattern parsers

pConsPattern :: Parser Pat
pConsPattern = do
  -- Cons x y
  name <- pCtorName
  patterns <- many pPattern
  pure $ PCons name patterns

pTuplePattern :: Parser Pat
pTuplePattern = enclosed '(' ')' $ do
  -- (x, y)
  patterns <- pPattern `sepBy` symbol ","
  pure $ case patterns of
    [p] -> p -- Single pattern in parentheses is just that pattern
    ps -> PTuple ps -- A tuple pattern

pPartitionPattern :: Parser Pat
pPartitionPattern = do
  -- (x::xs)
  first <- pIdent <|> symbol "_"
  _ <- symbol "::"
  PPartition first <$> pPattern

pCapturePattern :: Parser Pat
pCapturePattern = PCapture <$> pIdent

pWildcardPattern :: Parser Pat
pWildcardPattern = PWildcard <$ symbol "_"

pPattern :: Parser Pat
pPattern =
  choice
    [ PLit <$> pLiteral
    , try pConsPattern
    , try pCapturePattern
    , try pPartitionPattern
    , try pTuplePattern
    , pWildcardPattern
    ]
