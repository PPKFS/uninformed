module Main where

import Test.Tasty
import qualified Uninformed.Test.Syntax.Zipper as Z
import qualified Uninformed.Test.Bulk as Bulk
import Uninformed.Test.Common

main :: IO ()
main = do
  lc <- readTestCases "test/Bulk/"
  --s2 <- Sentences.spec2 lc
  defaultMain $ testGroup "Tests"
    [ Bulk.spec lc
    , Z.spec
    ]