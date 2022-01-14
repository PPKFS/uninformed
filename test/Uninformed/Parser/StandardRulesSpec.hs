module Uninformed.Parser.StandardRulesSpec (spec) where

import Test.Hspec
import Uninformed.Prelude
import Uninformed.Parser.TestHelpers
import Uninformed.Parser.Driver
import Uninformed.Parser.Extensions

spec :: Spec
spec = pass
{-}
canParseEverything
  :: Text
  -> Extension
  -> Expectation
canParseEverything = canParse parseExtension

spec :: Spec
spec = do
  describe "the standard rules" $ do
    it "can parse the standard rules" $ do
      stdraw <- toText <$> readFile "test/standard_rules.txt"
      canParseEverything stdraw Extension
-}