module Uninformed.Test.Common
  ( HasPipeline(..)
  , Stage(..)

  , runTestSuite
  ) where
import Uninformed.Words.TextFromFiles
import Uninformed.Words.Lexer
import Uninformed.Words.Lexer.Types
import Uninformed.Words.Vocabulary
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Uninformed.Syntax.Sentences
import Uninformed.Syntax.Sentences.Break
import Uninformed.Syntax.SyntaxTree
import Uninformed.Syntax.Sentences.Arrange

data Stage =
  LexingStage
  | SentenceBreakingStage
  | SyntaxTreeArrangingStage

class HasPipeline (s :: Stage) where
  type PipelineOutput s
  runPipeline :: Proxy s -> Text -> Either Text (PipelineOutput s)

instance HasPipeline 'LexingStage where
  type PipelineOutput 'LexingStage = (SourceFile [InformWord], VocabMap)
  runPipeline _ sf = lex (LexerInput False Nothing sf)

instance HasPipeline 'SentenceBreakingStage where
  type PipelineOutput 'SentenceBreakingStage = [Sentence]
  runPipeline _ sf = do
    (sf', _) <- lex (LexerInput False Nothing sf)
    pure $ breakIntoSentences (_sourceFileData sf')

instance HasPipeline 'SyntaxTreeArrangingStage where
  type PipelineOutput 'SyntaxTreeArrangingStage = SyntaxTree ()
  runPipeline _ sf = do
    (sf', _) <- lex (LexerInput False Nothing sf)
    let s = breakIntoSentences (_sourceFileData sf')
    pure $ createSyntaxTreeSkeleton (fromMaybe "aaaa" $ _sourceFileName sf') s

runTestSuite ::
  forall s b.
  HasPipeline s
  => [(FilePath, Text)]
  -> String
  -> String
  -> Proxy (s :: Stage)
  -> (Text -> b)
  -> (b -> PipelineOutput s -> Assertion)
  -> IO TestTree
runTestSuite allFiles grpName exemplarPrefix _ parseExemplar compareExemplar = do
  let fps = map fst allFiles
  allFiles2 <- mapM (fmap decodeUtf8 . readFileBS . (exemplarPrefix </>)) fps
  let cmb = zip3
        (map (takeFileName . dropExtensions) fps)
        (map snd allFiles)
        (map parseExemplar allFiles2)
  return $ testGroup grpName $
    flip map cmb $ \(fp, f, e) ->
      testCase fp $ do
        let res = runPipeline (Proxy @s) f
        case res of
          Left err -> assertFailure $ toString err
          Right sf -> compareExemplar e sf