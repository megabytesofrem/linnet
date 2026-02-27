module Main (main) where

import ParserTests (parserTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain parserTests
