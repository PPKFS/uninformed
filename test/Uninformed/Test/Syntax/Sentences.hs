module Uninformed.Test.Syntax.Sentences
  ( spec
  ) where
import Test.Tasty
import Uninformed.Words.Lexer
import System.FilePath ( (</>), dropExtensions, takeFileName )
import Test.Tasty.HUnit
    ( testCase, (@=?), assertFailure, Assertion )
import Uninformed.Syntax.Sentences ( breakIntoSentences )
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T
import Uninformed.Words.TextFromFiles
    ( SourceFile(..) )

spec :: [(FilePath, Text)] -> IO TestTree
spec allFiles = do
  let prfx2 = "test/Uninformed/Test/Syntax/Expected"
      fps = map fst allFiles
  allFiles2 <- mapM (fmap decodeUtf8 . readFileBS . (prfx2 </>)) fps
  let cmb = zip3
        (map (takeFileName . dropExtensions) fps)
        (map snd allFiles)
        (map parseExemplar allFiles2)
  return $ testGroup "Sentence Breaking" $
    flip map cmb $ \(fp, f, e) ->
      testCase ("breaks sentences for " <> fp) $ do
        let res = lex False Nothing f
        case res of
          Left err -> assertFailure $ error . toText . errorBundlePretty $ err
          Right (sf, vm) -> compareSentenceInfo (breakIntoSentences (_sourceFileData sf)) e

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

parseExemplar :: Text -> (Int, [Text])
parseExemplar t = let l = filter isRelevantSentenceNode . map T.stripStart . lines $ t in (length l, l)

isRelevantSentenceNode :: Text -> Bool
isRelevantSentenceNode t = "SENTENCE_NT" `T.isPrefixOf` t || ("HEADING_NT" `T.isPrefixOf` t && not ("0}" `T.isSuffixOf` t))

compareSentenceInfo :: [WordList] -> (Int, [Text]) -> Assertion
compareSentenceInfo wl (numSentences, fullSentenceList) = if
  numSentences /= length wl
  then
    let zipL = zipWithPadding "" "" fullSentenceList (map (T.intercalate " " . map displayWord) wl) in
    assertFailure $ toString $ "Failed to match the right number of sentences, and instead we got \n" <>
      unlines (map show zipL) <> " exp" <> show numSentences <> " act" <> show (length wl)
  else
    numSentences @=? length wl
