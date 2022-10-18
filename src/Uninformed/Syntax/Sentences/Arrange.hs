module Uninformed.Syntax.Sentences.Arrange where

import Uninformed.Syntax.SyntaxTree
import Uninformed.Words.Lexer.Types
import Uninformed.Syntax.Sentences

data HeadingType = Implicit | Explicit

createSyntaxTreeSkeleton ::
  Text
  -> [Sentence]
  -> SyntaxTree Word
createSyntaxTreeSkeleton fn = fst . unzipper . foldl' acceptSentence (newSyntaxTree fn)

acceptSentence ::
  Zipper Word
  -> Sentence
  -> Zipper Word
acceptSentence z sentence = do
  detectChangeOfSourceFile z sentence

  -- if this sentence is in a different file to our current focus, then
  -- we want a new implicit super heading.
detectChangeOfSourceFile ::
  Zipper Word
  -> Sentence
  -> Zipper Word
detectChangeOfSourceFile z sentence = if lastFile /= sentenceFileOfOrigin sentence
  then
    makeNewHeadingNode Implicit 0 (sentenceFileOfOrigin sentence) z
  else
    z
  where
    lastFile = focus z ^. nodeLocation % sourceLocationFile

makeNewHeadingNode :: 
  HeadingType 
  -> Int 
  -> Maybe Text 
  -> Zipper Word
  -> Zipper Word
makeNewHeadingNode ht hl hfn z = 
  upWhile (\z' -> getHeadingLevel z' > hl) z & 
  graftNodeChild (newNode)

newNode :: Node Word
newNode = _
  -- if the new heading level is lower than the current heading level, then
  -- we need to go *up* until we hit a higher level.


