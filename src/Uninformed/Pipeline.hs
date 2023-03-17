module Uninformed.Pipeline where

import Uninformed.Prelude
import Error.Diagnose
import Uninformed.Lexer.Run
import Uninformed.Syntax.Sentences
import Uninformed.Syntax.Sentences.Arrange
import Uninformed.Syntax.Sentences.Break
import Uninformed.Syntax.SyntaxTree
import Uninformed.Word
import Uninformed.Words.TextFromFiles
import Uninformed.Words.Vocabulary
import qualified Control.Monad.State.Strict as S

data Source = Source (Maybe Text) Text Bool
data Stage =
  LexingStage
  | SentenceBreakingStage
  | SyntaxTreeArrangingStage

class HasPipeline (s :: Stage) where
  type PipelineOutput s
  type PipelineInput s
  runPipeline :: Proxy s -> Source -> Either (Diagnostic Text) (PipelineOutput s)
  runPipelineStage :: Proxy s -> PipelineInput s -> Either (Diagnostic Text) (PipelineOutput s)

instance HasPipeline 'LexingStage where
  type PipelineInput 'LexingStage = Source
  type PipelineOutput 'LexingStage = (SourceFile [Word], VocabMap)
  runPipeline _ (Source mbName sf splitQuotes) = lex (LexerInput splitQuotes mbName sf)
  runPipelineStage = runPipeline

instance HasPipeline 'SentenceBreakingStage where
  type PipelineOutput 'SentenceBreakingStage = [Sentence]
  type PipelineInput 'SentenceBreakingStage = SourceFile [Word]
  runPipeline _ sf = do
    (sf', _) <- runPipelineStage (Proxy @'LexingStage) sf
    runPipelineStage (Proxy @'SentenceBreakingStage) sf'
  runPipelineStage _ sf = do
    pure $ S.evalState breakIntoSentences (defaultSentenceBreakerState sf)

instance HasPipeline 'SyntaxTreeArrangingStage where
  type PipelineOutput 'SyntaxTreeArrangingStage = SyntaxTree ()
  type PipelineInput 'SyntaxTreeArrangingStage = (SourceFile [Word], [Sentence])
  runPipeline _ sf = do
    (sf', _) <- runPipelineStage (Proxy @'LexingStage) sf
    s <- runPipelineStage (Proxy @'SentenceBreakingStage) sf'
    runPipelineStage (Proxy @'SyntaxTreeArrangingStage) (sf', s)
  runPipelineStage _ (sf', s) = do
    pure $ createSyntaxTreeSkeleton (fromMaybe "<interactive>" $ sourceFileName sf') s