module Uninformed.SentencifierSpec (spec) where

import Relude
import Uninformed.Lexer
import Uninformed.Sentencifier
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Test.Hspec

exs = " \"Title\" \n \n Some text here. nopbreak. \n \"a quoted string\" and then pbreak. \n \n \n \"a quoted string [with sub]\"."
spec :: Spec
spec = describe "Checking the amount of sentences" $ do
    -- it should do 1 paragraph, or multiple
    -- it should correctly mark the number of sentences in some paragraphs.

    it "idk" $ do
        -- I guess it's mostly checking that it has X sentences
        let v = parse (runReaderT Uninformed.Lexer.sourceToLexemes Uninformed.Lexer.defaultLexerSettings) "" exs
        case v of
            Left err -> expectationFailure $ show (bundleErrors err)
            Right s -> do
                let s' = parse (Uninformed.Sentencifier.lexemesToSentences) "" s
                case s' of
                    Left err -> expectationFailure $ show (bundleErrors err)
                    Right s -> expectationFailure $ "it worked" <> show s