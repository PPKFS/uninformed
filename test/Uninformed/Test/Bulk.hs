module Uninformed.Test.Bulk where
import Uninformed.Pipeline
import Test.Tasty.HUnit
import Test.Tasty
import Error.Diagnose
import System.FilePath
import Text.Megaparsec
import qualified Uninformed.Test.Words.Lexer as Lexer
import qualified Uninformed.Test.Syntax.Sentences as Sentences

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
          Left err' -> assertFailure $ show $ prettyDiagnostic True 4 err'
          Right sf -> compareExemplar e sf

data AllOutput = AllOutput
  { lexingOutput :: PipelineOutput 'LexingStage
  , sentenceOutput :: PipelineOutput 'SentenceBreakingStage

  }

spec ::
  [(FilePath, Text)]
  -> TestTree
spec allFiles = testGroup "Bulk golden tests" $
    flip map allFiles $ \(fp, f) -> testCase fp $ do
      let mbOutput = do
            lo@(sf, _) <- runPipelineStage @'LexingStage Proxy f
            sentences <- runPipelineStage (Proxy @'SentenceBreakingStage) sf
            pure $ AllOutput lo sentences
      case mbOutput of
        Left err' -> assertFailure $ show $ prettyDiagnostic True 4 err'
        Right output -> do
          checkLexing (lexingOutput output) fp
          checkSentences (sentenceOutput output) fp

checkLexing :: PipelineOutput 'LexingStage -> FilePath -> IO ()
checkLexing (sf, _) fp = do
  fileContents <- fmap decodeUtf8 . readFileBS . ("test/Bulk/Lexer" </>) $ fp
  let v = either (error . toText . errorBundlePretty) id . parse Lexer.parseExemplar "" $ fileContents
  Lexer.compareLexerInfo v $ Lexer.getLexerInfo sf

checkSentences :: PipelineOutput 'SentenceBreakingStage -> FilePath -> IO ()
checkSentences s fp = do
  fileContents <- fmap decodeUtf8 . readFileBS . ("test/Bulk/Sentences" </>) $ fp
  let v = Sentences.parseExemplar fileContents
  Sentences.compareSentenceInfo v s