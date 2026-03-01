module Linnet.Prettyprint
  ( Prettyprint (..)
  , prettyPrintWith
  , prettyPrintFoldable
  , withIndent
  , getIndent
  , evalPrettyPrint
  , runPrettyPrint
  )
where

import Control.Monad.State

-- State for the pretty printer
newtype PrettyprintState = PrettyprintState
  { indentLevel :: Int
  }

-- Prettyprinter monad to keep track of indentation and state
type Prettyprinter a = State PrettyprintState a

prettyPrintWith
  :: (Prettyprint a, Traversable t)
  => (a -> Prettyprinter String)
  -> t a
  -> Prettyprinter (t String)
prettyPrintWith = traverse

prettyPrintFoldable
  :: (Prettyprint a, Traversable t)
  => t a
  -> Prettyprinter (t String)
prettyPrintFoldable = traverse pretty

indentsToSpaces :: PrettyprintState -> String
indentsToSpaces p = replicate (indentLevel p * 2) ' '

getIndent :: Prettyprinter String
getIndent = gets indentsToSpaces

withIndent :: Prettyprinter String -> Prettyprinter String
withIndent action = do
  oldLevel <- gets indentLevel
  modify $ \s -> s{indentLevel = oldLevel + 1}
  result <- action
  modify $ \s -> s{indentLevel = oldLevel}
  pure result

-- Typeclass for pretty printing AST nodes
class Prettyprint a where
  pretty :: a -> Prettyprinter String

-- Evaluate a single step of the pretty printer and return the resulting string
evalPrettyPrint :: (Prettyprint a) => a -> String
evalPrettyPrint item = evalState (pretty item) (PrettyprintState 0)

-- Run the pretty printer and return the resulting string along with the final state
runPrettyPrint :: (Prettyprint a) => a -> PrettyprintState -> (String, PrettyprintState)
runPrettyPrint item = runState (pretty item)