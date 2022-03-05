module Main ( main ) where


import qualified Uninformed.Parser.ExtensionsSpec as Extensions
import qualified Uninformed.Parser.HeadingsSpec as Headings

import Uninformed.Prelude
import Test.Hspec
import qualified Uninformed.Parser.StandardRulesSpec as StdRules

main :: IO ()
main = hspec $ do
  describe "Headings" Headings.spec
  describe "Extensions" Extensions.spec
  describe "The Standard Rules" StdRules.spec
