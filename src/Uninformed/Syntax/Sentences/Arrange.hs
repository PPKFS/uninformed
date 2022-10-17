module Uninformed.Syntax.Sentences.Arrange where

import Uninformed.Syntax.SyntaxTree
import Uninformed.Words.Lexer
import Uninformed.Syntax.Sentences


createSyntaxTreeSkeleton ::
  Text
  -> [Sentence]
  -> SyntaxTree Word
createSyntaxTreeSkeleton fn = fst . foldl' acceptSentence (newSyntaxTree fn)

acceptSentence ::
  Zipper Word
  -> Sentence
  -> Zipper Word
acceptSentence z sentence = do
  detectChangeOfSourceFile z sentence

detectChangeOfSourceFile ::
  Zipper Word
  -> Sentence
  -> Zipper Word
detectChangeOfSourceFile z sentence = if lastFile /= sentenceFileOfOrigin sentence
  then
    makeNewHeadingNode Implicit 0 (sentenceFileOfOrigin sentence)
  else
    z
  where
    lastFile = focus z ^. nodeSourceLocation


  --detectDividingSentence parsedSentence
  -- some skipping checks and also some problems
  --checkForSkipping
  --considerHeadingNode

  -- if this sentence is in a different file to our current focus, then
  -- we want a new implicit super heading.
