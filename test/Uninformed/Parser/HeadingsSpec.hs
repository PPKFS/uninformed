module Uninformed.Parser.HeadingsSpec
  ( spec ) where

import Test.Hspec
import Uninformed.Prelude
import Test.Hspec.Megaparsec
import Uninformed.Parser.Headings
import Uninformed.Parser.Driver (runParser, Parser)
import Uninformed.Parser.Extensions (Extension(..))
import Text.Megaparsec (ParseError, ErrorFancy(..) )
import Uninformed.Parser.Common
import Test.Hspec (pendingWith)

canParse
  :: Show a 
  => Eq a
  => Parser a
  -> Text
  -> a
  -> Expectation
canParse p i o = do
  r <- runParser p i
  r `shouldParse` o

cannotParse
  :: Show a 
  => Parser a
  -> Text
  -> ParseError Text UninformedParseError
  -> Expectation
cannotParse p i e = cannotParseM p i [e]

cannotParseM
  :: Show a 
  => Parser a
  -> Text
  -> [ParseError Text UninformedParseError]
  -> Expectation
cannotParseM p i e = do
  r <- runParser p i
  r `shouldFailWithM` e

canParseHeading
  :: Text
  -> Heading
  -> Expectation
canParseHeading = canParse parseHeading

cannotParseHeading
  :: Text
  -> ParseError Text UninformedParseError
  -> Expectation
cannotParseHeading t p = cannotParseHeadingM t [p]

cannotParseHeadingM
  :: Text
  -> [ParseError Text UninformedParseError]
  -> Expectation
cannotParseHeadingM = cannotParseM parseHeading

-- I should start a list of things I do differently to inform.
-- punctuation is fine in heading quotes.

spec :: Spec
spec = do
  describe "Regular headings" $ do
    it "can parse a heading with no name" $
      canParseHeading "Chapter 1" (makeHeading' Chapter "1") 
    it "can parse normal headings" $ do
      canParseHeading "Book 5 Test" (makeHeading' Book "5 Test")
      canParseHeading "volume 5 - Test" (makeHeading' Volume "5 - Test")
      canParseHeading "part--------Test" (makeHeading' Part "Test")
    it "removes excess whitespace" $
      canParseHeading "part                      2 test               " (makeHeading' Part "2 test")
    it "doesn't handle punctuation" $ do
      cannotParseHeading "Chapter 6 with ; forbidden punctuation"  $ errFancy 15 $ fancy (ErrorCustom (PunctuationInHeading ';'))
      cannotParseHeading "Chapter 6 With A Full Stop." $ errFancy 26 $ fancy (ErrorCustom (PunctuationInHeading '.'))
    it "handles quotes with punctutation though" $ do
      canParseHeading "Part 3 \" test: \"" (makeHeading' Part "3 \" test: \"")
      canParseHeading "Volume \"...\""  (makeHeading' Volume "\"...\"")
    it "or mismatched quotes" $
      cannotParseHeading "Section a \" uh"  $ err 14 (ueof <> etok '"')
    it "doesn't handle headings with no marker" $
      cannotParseHeading "Book" $ err 4 (elabel "heading name") <> err 4 ueof
      
  describe "Headings with extra gubbins" $ do
    
    it "can handle for/not for release" $ do
      canParseHeading "Section 1 - F - Not For Release" ((makeHeading' Section "1 - F") { _headingForRelease = Just NotForRelease })
      canParseHeading "Section F - for ReleAse OnLy" ((makeHeading' Section "F") { _headingForRelease = Just ForReleaseOnly })
    
    it "can parse for use with(out)" $ do
      canParseHeading "Chapter 2a (for use with Locksmith by Emily Short)" (((makeHeading' Chapter "2a") { _headingUseWith = Just $ UseWith (Extension "Locksmith" "Emily Short") }))
      canParseHeading "Chapter 2a (for use without Locksmith2 by Emily Short)" 
        (((makeHeading' Chapter "2a") { _headingUseWith = Just $ UseWithout (Extension "Locksmith2" "Emily Short") }))
    
    it "can parse in place of" $ do
      canParseHeading 
        "Section 6 - Hacked locking (in place of Section 1 - Regular locking in Locksmith by Emily Short)"
        ((makeHeading' Section "6 - Hacked locking") 
          { _headingInPlaceOf = Just $ InPlaceOf (HeadingName Section "1 - Regular locking") (Extension "Locksmith" "Emily Short") })
    it "doesn't read an incomplete heading ending wrongly" $ 
      canParseHeading "Volume 2 uninde (in place o" (makeHeading' Volume "2 uninde (in place o")
    it "can parse unindexed" $ do
      canParseHeading "Volume 2 unindexed" ((makeHeading' Volume "2") { _headingIsIndexed = False })
    
    it "can parse in place of with quotes" $
      canParseHeading 
        "Section - Hacked questions (in place of \"Section 4 - Phrase used to ask questions in closed mode\" in Questions by Michael Callaghan)"
        ((makeHeading' Section "Hacked questions") 
          { _headingInPlaceOf = Just $ InPlaceOf (HeadingName Section "4 - Phrase used to ask questions in closed mode") 
            (Extension "Questions" "Michael Callaghan") })

