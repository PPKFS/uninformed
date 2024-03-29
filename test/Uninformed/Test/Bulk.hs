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
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)

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
      unless (fp `elem` isIgnoredForNow) $ do
        let mbOutput = do
              lo@(sf, _) <- runPipelineStage @'LexingStage Proxy (Source (Just . toText $ fp) f False)
              sentences <- runPipelineStage (Proxy @'SentenceBreakingStage) sf
              pure $ AllOutput lo sentences
        case mbOutput of
          Left err' -> checkError isBless fp err'
          Right output -> do
            Lexer.checkLexing isBless (lexingOutput output) fp
            checkSentences (sentenceOutput output) fp

isIgnoredForNow :: [FilePath]
isIgnoredForNow =
  [ "PM_ChoiceSelectionMissing.txt" -- dialogue
  , "PM_NegationForbidden.txt" -- doesn't handle 14:-4 right
  , "PM_HeadingStopsBeforeEndOfLine.txt" -- fails in lexing
  , "PM_SemicolonAfterColon.txt", "PM_ParaEndsInColon.txt", "PM_SemicolonAfterStop.txt"-- sentence break error handling
  , "PM_SentenceEndsInSemicolon.txt", "PM_SentenceEndsInColon.txt"
  , "PM_TableColumnLocation.txt" -- weird table parsing fail
  , "I6StringEscapes-G.txt" -- weird i6 parsing fail
  , "PM_ChoiceLeftArrowExpected.txt", "LBW.txt", "PM_ChoiceDividerDependent.txt" -- dialogue
  , "Dialogue.txt" -- dialogue
  ]

checkError ::
  Bool
  -> FilePath
  -> Diagnostic Text
  -> Assertion
checkError False fp diag = do
  let filename = "test/Bulk/Problems" </> fp -<.> "json"
  exists <- doesFileExist filename
  if exists
  then do
    fileContents <- readFileBS filename
    if BS.empty == fileContents
    then checkError True fp diag
    else
      assertEqual "Error messages were not equal: " fileContents (toStrict $ diagnosticToJson diag)
  else checkError True fp diag
checkError True fp diag = encodeFile ("test/Bulk/Problems" </> fp -<.> "json") diag

checkSentences ::
  PipelineOutput 'SentenceBreakingStage
  -> FilePath
  -> IO ()
checkSentences s fp = do
  fileContents <- fmap decodeUtf8 . readFileBS . (\x -> "test/Bulk/Sentences" </> x -<.> "txt") $ fp
  Sentences.compareSentenceInfo (Sentences.parseExemplar fileContents) s