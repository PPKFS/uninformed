module Uninformed.Test.Syntax.Sentences
  ( spec
  , spec2
  ) where
import Test.Tasty
import Uninformed.Words.Lexer
import Test.Tasty.HUnit
    ( (@=?), assertFailure, Assertion )
import qualified Data.Text as T
import Uninformed.Syntax.Sentences
import Uninformed.Test.Common
import Uninformed.Syntax.SyntaxTree

spec :: [(FilePath, Text)] -> IO TestTree
spec allFiles =
  let prfx2 = "test/Uninformed/Test/Syntax/Expected"
  in
    runTestSuite allFiles "Sentence Breaking" prfx2 (Proxy @'SentenceBreakingStage)
      parseExemplar compareSentenceInfo

spec2 :: [(FilePath, Text)] -> IO TestTree
spec2 allFiles =
  let prfx2 = "test/Uninformed/Test/Syntax/Expected"
  in
    runTestSuite allFiles "Sentence Arranging" prfx2 (Proxy @'SyntaxTreeArrangingStage)
      parseExemplar ensureAtLeastOneHeading

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

parseExemplar :: Text -> (Int, [Text])
parseExemplar t = let l = filter isRelevantSentenceNode . map T.stripStart . lines $ t in (length l, l)

isRelevantSentenceNode :: Text -> Bool
isRelevantSentenceNode t = "SENTENCE_NT" `T.isPrefixOf` t || ("HEADING_NT" `T.isPrefixOf` t && not ("0}" `T.isSuffixOf` t))

compareSentenceInfo :: (Int, [Text]) -> [Sentence] -> Assertion
compareSentenceInfo (numSentences, fullSentenceList) wl = if
  numSentences /= length wl
  then
    let zipL = zipWithPadding "" "" fullSentenceList (map (T.intercalate " " . map displayWord . toList . unSentence) wl) in
    assertFailure $ toString $ "Failed to match the right number of sentences, and instead we got \n" <>
      unlines (map show zipL) <> " exp" <> show numSentences <> " act" <> show (length wl)
  else
    numSentences @=? length wl

ensureAtLeastOneHeading :: (Int, [Text]) -> SyntaxTree () -> Assertion
ensureAtLeastOneHeading _ s = assertFailure (show s)