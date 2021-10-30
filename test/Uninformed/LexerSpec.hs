module Uninformed.LexerSpec (spec) where

import Relude
import Uninformed.Lexer
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Test.Hspec
import Text.RawString.QQ

lexAndSentenceTesting :: Text
lexAndSentenceTesting = [r|   "Title with whitespace"

  Hello. This is some text: And some more.
Then we have a newline.

    And a paragraph break;"quoted text" "quoted text with an end;""qu[sub][s][s]    "

The end;;;;......

"H"
|]
parseSucceeds :: Text -> Expectation
parseSucceeds = shouldSucceedOn
    $ parse (runReaderT Uninformed.Lexer.sourceToLexemes Uninformed.Lexer.defaultLexerSettings) ""
parseFails :: Text -> Expectation
parseFails = shouldFailOn
    $ parse (runReaderT Uninformed.Lexer.sourceToLexemes Uninformed.Lexer.defaultLexerSettings) ""
spec :: Spec
spec = do
    describe "Lexing valid strings" $ do
        it "parses empty string" $ parseSucceeds ""
        it "parses a regular file" $ parseSucceeds lexAndSentenceTesting
    describe "Lexing invalid strings" $ do
        it "Fails with dropped ]" $ parseFails "\"blah [oops I dropped "
        it "Fails with dropped \"" $ parseFails "oh no I. ; \""
        it "Fails on odd inputs" $ parseFails "lll[a{{{{{{{{"