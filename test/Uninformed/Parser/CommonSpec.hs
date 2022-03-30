module Uninformed.Parser.CommonSpec (spec) where

import Test.Hspec
import Solitude

spec :: Spec
spec = pass
{-

canParseWord
  :: Text
  -> Text
  -> Expectation
canParseWord = canParse (do
  e <- parseAnyWord
  eof
  return e)

cannotParseWord
  :: Text
  -> ParseError Text UninformedParseError
  -> Expectation
cannotParseWord = cannotParse (do
  void parseAnyWord
  eof)

spec :: Spec
spec = do
  describe "parsing excess whitespace" $ do
    it "can parse a word with a newline after it" $
      canParseWord "foo              \n         " "foo"
    it "cannot parse a word with a newline paragraph break after it" $

      cannotParseWord "foo \n \n    "
        (errFancy 7 $ fancy $ ErrorCustom UnexpectedParagraphBreak)
-}