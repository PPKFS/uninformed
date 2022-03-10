module Uninformed.Parser.ExtensionsSpec (spec) where

import Test.Hspec
import Uninformed.Prelude
import Uninformed.Extensions.Parser
import Uninformed.Parser.TestHelpers
import Uninformed.Parser.Types
import Optics
import Uninformed.Extensions.Types
import Uninformed.Parser.Expressions

initExtensionParser
  :: ParseState
  -> ParseState
initExtensionParser = (snippetHandler % snippetFilename) .~ extensionHeadingTestFile
-- I should start a list of things I do differently to inform.
-- punctuation is fine in heading quotes.

extensionHeadingTestFile :: Text
extensionHeadingTestFile = "ExtensionHeadings.tests"

canParseExtensionHeader
  :: Text
  -> ExtensionHeader
  -> Expectation
canParseExtensionHeader t e = canParse initExtensionParser parseExtensionHeader (t <> "\n\n") (ExtensionHeaderExpr e)

spec :: Spec
spec = do
  describe "extension headings" $ do
    it "can parse a regular heading" $
      canParseExtensionHeader "Locksmith Extra by Emily Short begins here."
        (ExtensionHeader (ExtensionName "Locksmith Extra" "Emily Short") Nothing)
    it "can parse a heading with a short version string" $
      canParseExtensionHeader "Version 2 of Locksmith Extra by Emily Short begins here."
        (ExtensionHeader (ExtensionName "Locksmith Extra" "Emily Short") (Just (ExtensionVersion 2 Nothing)) )
    it "can parse a heading with a serial version string" $
      canParseExtensionHeader "Version 060430 of Locksmith Extra by Emily Short begins here."
        (ExtensionHeader (ExtensionName "Locksmith Extra" "Emily Short") (Just (ExtensionVersion 1 (Just 60430))) )
    it "can parse a heading with a full version string" $
      canParseExtensionHeader "Version 2/060430 of Locksmith Extra by Emily Short begins here."
        (ExtensionHeader (ExtensionName "Locksmith Extra" "Emily Short") (Just (ExtensionVersion 2 (Just 60430))) )