module Main (main) where

import ParserTests (parserTests)
import Test.Tasty (defaultMain, testGroup)
import TypecheckerTests (typecheckerTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Linnet Tests"
      [parserTests, typecheckerTests]
