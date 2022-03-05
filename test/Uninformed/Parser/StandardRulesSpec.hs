module Uninformed.Parser.StandardRulesSpec (spec) where

import Test.Hspec
import Uninformed.Prelude
import Uninformed.Parser.TestHelpers
import Uninformed.Parser.Driver
import Uninformed.Parser.Types
import Optics
import Uninformed.Extensions.Types

initHeadingParser
  :: ParseState
  -> ParseState
initHeadingParser = (snippetHandler % snippetFilename) .~ "aaaa"

canParseEverything
  :: Text
  -> ExtensionF (Fix ExprF)
  -> Expectation
canParseEverything t e = canParse initHeadingParser parseExtension (t <> "\n\n") (ExtensionExpr e)

spec :: Spec
spec = do
  describe "the standard rules" $ do
    it "can parse the standard rules" $ do
      stdraw <- toText <$> readFile "test/standard_rules.txt"
      canParseEverything stdraw Extension