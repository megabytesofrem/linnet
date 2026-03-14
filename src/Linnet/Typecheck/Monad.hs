module Linnet.Typecheck.Monad
  ( TypecheckM (..)
  ) where

import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader

--  Typechecking monad with access to a type environment
newtype TypecheckM env a = TypecheckM
  { runTypecheckM :: ReaderT env (Except String) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadReader env (TypecheckM env) where
  ask = TypecheckM ask
  local f (TypecheckM m) = TypecheckM (local f m)

instance MonadError String (TypecheckM env) where
  throwError = TypecheckM . throwError
  catchError (TypecheckM m) handler = TypecheckM $ catchError m (runTypecheckM . handler)

instance MonadFail (TypecheckM env) where
  fail = throwError
