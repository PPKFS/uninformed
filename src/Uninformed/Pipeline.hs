module Uninformed.Pipeline where
import Error.Diagnose
import Uninformed.Words.TextFromFiles
import Uninformed.Words.Lexer
import Uninformed.Words.Vocabulary
import Uninformed.Syntax.Sentences
import Uninformed.Syntax.SyntaxTree
import Uninformed.Words.Lexer.Types
import Uninformed.Syntax.Sentences.Break
import Uninformed.Syntax.Sentences.Arrange

data Stage =
  LexingStage
  | SentenceBreakingStage
  | SyntaxTreeArrangingStage

class HasPipeline (s :: Stage) where
  type PipelineOutput s
  type PipelineInput s
  runPipeline :: Proxy s -> Text -> Either (Diagnostic Text) (PipelineOutput s)
  runPipelineStage :: Proxy s -> PipelineInput s -> Either (Diagnostic Text) (PipelineOutput s)

instance HasPipeline 'LexingStage where
  type PipelineInput 'LexingStage = Text
  type PipelineOutput 'LexingStage = (SourceFile [InformWord], VocabMap)
  runPipeline _ sf = lex (LexerInput False Nothing sf)
  runPipelineStage = runPipeline

instance HasPipeline 'SentenceBreakingStage where
  type PipelineOutput 'SentenceBreakingStage = [Sentence]
  type PipelineInput 'SentenceBreakingStage = SourceFile [InformWord]
  runPipeline _ sf = do
    (sf', _) <- runPipelineStage (Proxy @'LexingStage) sf
    runPipelineStage (Proxy @'SentenceBreakingStage) sf'
  runPipelineStage _ sf = do
    pure $ breakIntoSentences (sourceFileData sf)

instance HasPipeline 'SyntaxTreeArrangingStage where
  type PipelineOutput 'SyntaxTreeArrangingStage = SyntaxTree ()
  type PipelineInput 'SyntaxTreeArrangingStage = (SourceFile [InformWord], [Sentence])
  runPipeline _ sf = do
    (sf', _) <- runPipelineStage (Proxy @'LexingStage) sf
    s <- runPipelineStage (Proxy @'SentenceBreakingStage) sf'
    runPipelineStage (Proxy @'SyntaxTreeArrangingStage) (sf', s)
  runPipelineStage _ (sf', s) = do
    pure $ createSyntaxTreeSkeleton (fromMaybe "<interactive>" $ sourceFileName sf') s