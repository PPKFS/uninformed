module Uninformed.Parser.TestHelpers
( canParse
, cannotParse
--, cannotParseM

) where

import Solitude
import Uninformed.Parser.Types
import Uninformed.Parser.Driver (runParser)
import Text.Megaparsec hiding ( runParser )
import qualified Data.Text as T
import Chapelure.Types
import Chapelure.Handler.PlainText
import qualified Data.Vector.NonEmpty as NEVec
import qualified Data.Vector as Vec
import qualified Control.Exception as E
import GHC.Stack ( SrcLoc )
import Test.Hspec (Expectation, expectationFailure)
import Text.Pretty.Simple (pShow, pShowNoColor)
import Test.HUnit.Lang hiding (Error)
import Uninformed.Parser.Errors
import Uninformed.Recursion
import Uninformed.Parser.Expressions

shouldParse
  :: HasCallStack
  => VisualStream s
  => Eq a
  => Show a
  => Either (ParseErrorBundle s UninformedParseError) a
  -> a
  -> Expectation
r `shouldParse` v = case r of
  Left e ->
    expectationFailure . toString $
      "Expected successful parse with result: " <> (toStrict . pShow) v
        <> "\nbut parsing failed with error:\n"
        <> showBundle e
  Right x -> v `shouldBe` x

-- | Asserts that the specified actual value is equal to the expected value.
-- slimmed down and modified to avoid a `Show` constraint.
shouldBeEx
  :: HasCallStack
  => Eq a
  => (a -> String)
  -> a      -- ^ The expected value
  -> a      -- ^ The actual valu
  -> Expectation
shouldBeEx sh expected actual = unless (actual == expected) $ do
    prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure lo $ ExpectedButGot prefaceMsg expectedMsg actualMsg)
  where
    prefaceMsg = Nothing :: Maybe String
    expectedMsg = sh expected
    actualMsg = sh actual
    lo :: HasCallStack => Maybe SrcLoc
    lo = case reverse (getCallStack callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

shouldBe
  :: HasCallStack
  => Eq a
  => Show a
  => a
  -> a
  -> Expectation
shouldBe = shouldBeEx (toString . pShowNoColor)
-- | Render a parse error bundle in a way that is suitable for inserting it
-- in a test suite report.

showBundle
  :: VisualStream a
  => ParseErrorBundle a UninformedParseError
  -> Text
showBundle = unlines . fmap indent . lines . toText . render . errorBundleToDiagnostic
  where
    indent x =
      if T.empty == x
        then x
        else "  " <> x

errorBundleToDiagnostic
  :: VisualStream a
  => ParseErrorBundle a UninformedParseError
  -> Diagnostic
errorBundleToDiagnostic (ParseErrorBundle es _) = foldl' (\d -> \case
  t@(TrivialError i _ _) -> d { help = Just $ toText $ parseErrorPretty t <> "at offset " <> show i}
  FancyError _ fes -> let (ds, sn) = errorFancyToSnippetVec fes in
    d {
    help = if T.empty /= mconcat ds then Just (fromMaybe "" (help d) <> mconcat ds) else Nothing,
    snippets = Just $ maybe
      (NEVec.unsafeFromList sn)
      (\s -> s NEVec.++ NEVec.unsafeFromList sn) (snippets d) }
  )
  (Diagnostic {
    code = Nothing
  , severity = Error
  , link = Nothing
  , help = Nothing
  , snippets = Nothing
  }) es

errorFancyToSnippetVec :: Set (ErrorFancy UninformedParseError) -> ([Text], [Snippet])
errorFancyToSnippetVec = unzip . map (\case
  --todo
  ErrorFail s -> ("", Snippet { location=(toText s, Line 3, Column 1), highlights= Just $ NEVec.singleton (Source{ label = Just "This takes an “Int”"
                 , line = Line 4
                 , startColumn = Column 8
                 , endColumn = Column 8
                 }), content = Vec.fromList $ lines $ toText $ "add3 :: Int\nadd = 1 + True" <> s})
  --todo
  ErrorIndentation {} -> ("", Snippet { location=("Code.hs", Line 3, Column 1), highlights= Just $ NEVec.singleton (Source{ label = Just "This takes an “Int”"
                 , line = Line 4
                 , startColumn = Column 8
                 , endColumn = Column 8
                 }), content = Vec.fromList $ lines "addaaaaa :: Int\nadd = 1 + True"})
  ErrorCustom (UninformedParseError s dhlp) -> (dhlp, s)
  ) . toList

canParse
  :: HasCallStack
  => (ParseState -> ParseState)
  -> Parser ExprLoc
  -> Text
  -> ExprF (Fix ExprF)
  -> Expectation
canParse f p i o = do
  r <- runParser f (p <* finishParse) i
  fmap strip r `shouldParse` Fix o

finishParse :: Parser ()
finishParse = eof <|> unexpectedSentence

cannotParse
  :: HasCallStack
  => Show a
  => (ParseState -> ParseState )
  -> Parser a
  -> Text
  -> (Text -> Diagnostic)
  -> Expectation
cannotParse f p i e = do
  r <- runParser f p i
  r `shouldFailWith` (trimWhitespaceFromDiagnostic . e) i

trimWhitespaceFromDiagnostic :: Diagnostic -> Diagnostic
trimWhitespaceFromDiagnostic d@Diagnostic{..}= d { snippets = trimWhitespaceFromSnippets <$$> snippets}

trimWhitespaceFromSnippets :: Snippet -> Snippet
trimWhitespaceFromSnippets s@Snippet{..} = s { content = Vec.takeWhile (/= "") content}

shouldFailWith :: (HasCallStack, VisualStream a1, Show a2) => Either (ParseErrorBundle a1 UninformedParseError) a2 -> Diagnostic -> Expectation
shouldFailWith (Left peb) e = shouldBeEx (toString . render) e (errorBundleToDiagnostic peb)
shouldFailWith (Right ac) _ = expectationFailure $ toString (pShow ac)