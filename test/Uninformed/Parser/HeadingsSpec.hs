module Uninformed.Parser.HeadingsSpec
  ( spec ) where

import Solitude
import Uninformed.Headings.Parser
import Uninformed.Parser.Types
import Uninformed.Parser.TestHelpers
import Test.Hspec
import Chapelure.Types
import Optics
import Data.Text.Display
import Uninformed.Headings.Types
import Uninformed.Parser.Errors
import Uninformed.Extensions.Types
import Uninformed.Parser.Expressions
import Uninformed.Parser.Errors

canParseHeading
  :: HasCallStack
  => Text
  -> Heading
  -> Expectation
canParseHeading t h = canParse initHeadingParser parseHeading (t <> "\n\n") (HeadingExpr h)

cannotParseHeading
  :: HasCallStack
  => Text
  -> (Text -> Diagnostic)
  -> Expectation
cannotParseHeading t = cannotParse initHeadingParser parseHeading (t <> "\n\n")

initHeadingParser
  :: ParseState
  -> ParseState
initHeadingParser = (snippetHandler % snippetFilename) .~ headingTestFile
-- I should start a list of things I do differently to inform.
-- punctuation is fine in heading quotes.

headingTestFile :: Text
headingTestFile = "Headings.tests"

makeHeadingError
  :: Text
  -> [(Int, Int, Int, Text)]
  -> Text
  -> Diagnostic
makeHeadingError hlpMsg hl t = makeDiagnostic
  hlpMsg $
  buildSnippet headingTestFile t 1 (maybe 1 (\(_, x, _, _) -> x+1) (viaNonEmpty head hl)) hl

unknownHeading
  :: ParseErrorType
  -> Text
unknownHeading p = "In a heading declaration, " <> display p <> "."

makeHeading' ::
  HeadingLevel 
  -> Text 
  -> Heading 
makeHeading' lvl n = Heading (HeadingName lvl n) 0 defaultHeadingInfo

spec :: Spec
spec = do
  describe "Regular headings" $ do
    it "can parse a heading with no name" $
      canParseHeading "Chapter 1" (makeHeading' Chapter "1")
    it "can parse normal headings" $ do
      canParseHeading "Book 5 Test" (makeHeading' Book "5 Test")
      canParseHeading "volume 5 - Test" (makeHeading' Volume "5 - Test")
      canParseHeading "part ----~~~---- Test" (makeHeading' Part "Test")
    it "removes excess whitespace" $
      canParseHeading "part                      2 test           \n     " (makeHeading' Part "2 test")
    it "doesnt handle punctuation" $ do
      cannotParseHeading "Chapter 6 with ; forbidden punctuation"
        (makeHeadingError (unknownHeading UnexpectedToken) [(1, 15, 16, unexpectedPunctuationInHeadingMsg)])

      cannotParseHeading "Chapter 6 With A Full Stop."
        (makeHeadingError (unknownHeading UnexpectedToken) [(1, 26, 27, unexpectedPunctuationInHeadingMsg)])
    it "handles quotes with punctutation though" $ do
      canParseHeading "Part 3 \" test: \"" (makeHeading' Part "3 \" test: \"")
      canParseHeading "Volume \"...\""  (makeHeading' Volume "\"...\"")
    it "or mismatched quotes" $
      cannotParseHeading "Section a \" uh"
        (makeHeadingError (unknownHeading (MultipleParseErrors [UnexpectedToken, MissingQuoteEnd])) [(1, 14, 16, unexpectedNewlineMsg <> amendLiteralModeMsg)])
    it "or mismatched, unexpected but still valid punctuation" $
      cannotParseHeading "Volume 2 uninde (in place o" 
        (makeHeadingError (unknownHeading UnexpectedToken) [(1, 16, 17, phraseUnexpectedTokenMsg)])
    it "doesnt handle headings with no marker" $
      cannotParseHeading "Book"
        (makeHeadingError (unknownHeading UnexpectedToken) [(1, 4, 6, unexpectedNewlineMsg)])

  describe "Headings with extra gubbins" $ do

    it "can handle for/not for release" $ do
      canParseHeading "Section 1 - F - Not For Release" (
        makeHeading' Section "1 - F"  & (headingInfo % headingForRelease) ?~ NotForRelease)
      canParseHeading "Section F - for ReleAse OnLy" (
        makeHeading' Section "F" & (headingInfo % headingForRelease) ?~ ForReleaseOnly)

    it "can parse for use with(out)" $ do
      canParseHeading "Chapter 2a (for use with Locksmith by Emily Short)"  (
        makeHeading' Chapter "2a" & (headingInfo % headingUseWith) ?~ UseWith 
          (ExtensionName "Locksmith" "Emily Short"))
      canParseHeading "Chapter 2a (for use without Locksmith 2 by Emily Short)"  (
        makeHeading' Chapter "2a" & (headingInfo % headingUseWith) ?~ UseWithout 
          (ExtensionName "Locksmith 2" "Emily Short"))

    it "can parse in place of" $
      canParseHeading "Section 6 - Hacked locking (in place of Section 1 - Regular locking in Locksmith by Emily Short)" (
          makeHeading' Section "6 - Hacked locking" & (headingInfo % headingInPlaceOf) ?~ InPlaceOf 
            (HeadingName Section "1 - Regular locking") (ExtensionName "Locksmith" "Emily Short"))
    it "does not read an incomplete heading ending wrongly" $
      canParseHeading "Volume 2 uninde in place o" (makeHeading' Volume "2 uninde in place o")
    it "can parse unindexed" $
      canParseHeading "Volume 2 unindexed" 
        (makeHeading' Volume "2" & (headingInfo % headingIsIndexed) .~ False)

    it "can parse in place of with quotes" $
      canParseHeading 
        "Section - Hacked questions (in place of \"Section 4 - Phrase used to ask questions in closed mode\" in Questions by Michael Callaghan)"
        (makeHeading' Section "Hacked questions" & (headingInfo % headingInPlaceOf) ?~ InPlaceOf 
          (HeadingName Section "4 - Phrase used to ask questions in closed mode") (ExtensionName "Questions" "Michael Callaghan"))