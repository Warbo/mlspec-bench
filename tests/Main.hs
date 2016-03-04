module Main where

import MLSpec.Bench
import System.Process
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Command gets set" cmdIsSet
  ]

cmdIsSet s = case cmdspec (mkCmd s) of
  RawCommand c _ -> c === s
  _              -> error "Was expecting a RawCommand"
