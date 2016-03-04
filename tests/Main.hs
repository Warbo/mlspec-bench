module Main where

import MLSpec.Bench
import System.Process
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
      testProperty "Handle no args" readArgsNothing
    , testProperty "Handle args"    readArgsJust
  ]

readArgsNothing = readArgs Nothing === []

readArgsJust xs = readArgs (Just (show xs)) === xs
