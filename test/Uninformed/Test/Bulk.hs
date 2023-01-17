module Uninformed.Test.Bulk where


import Uninformed.Prelude
import Data.Aeson (encodeFile)

import Error.Diagnose ( Diagnostic, diagnosticToJson )
import System.FilePath ( (-<.>), (</>) )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( Assertion, testCase, assertEqual )
import Uninformed.Pipeline
import qualified Uninformed.Test.Syntax.Sentences as Sentences
import qualified Uninformed.Test.Words.Lexer as Lexer

data AllOutput = AllOutput
  { lexingOutput :: PipelineOutput 'LexingStage
  , sentenceOutput :: PipelineOutput 'SentenceBreakingStage
  }

spec ::
  Bool
  -> [(FilePath, Text)]
  -> TestTree
spec isBless allFiles = testGroup "Bulk golden tests" $
    flip map allFiles $ \(fp, f) -> testCase fp $ do
      let mbOutput = do
            lo@(sf, _) <- runPipelineStage @'LexingStage Proxy (Source (Just . toText $ fp) f False)
            sentences <- runPipelineStage (Proxy @'SentenceBreakingStage) sf
            pure $ AllOutput lo sentences
      case mbOutput of
        Left err' -> checkError isBless fp err'
        Right output -> do
          Lexer.checkLexing isBless (lexingOutput output) fp
          checkSentences (sentenceOutput output) fp

checkError ::
  Bool
  -> FilePath
  -> Diagnostic Text
  -> Assertion
checkError False fp diag = do
  fileContents <- readFileBS ("test/Bulk/Problems" </> fp -<.> "json")
  assertEqual "Error messages were not equal: " fileContents (toStrict $ diagnosticToJson diag)
checkError True fp diag = encodeFile ("test/Bulk/Problems" </> fp -<.> "json") diag

checkSentences ::
  PipelineOutput 'SentenceBreakingStage
  -> FilePath
  -> IO ()
checkSentences s fp = do
  fileContents <- fmap decodeUtf8 . readFileBS . (\x -> "test/Bulk/Sentences" </> x -<.> "txt") $ fp
  Sentences.compareSentenceInfo (Sentences.parseExemplar fileContents) s