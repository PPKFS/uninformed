module Uninformed.Syntax.Sentences.Arrange where

import Uninformed.Syntax.SyntaxTree
import Uninformed.Words.Lexer.Types
import Uninformed.Syntax.Sentences

createSyntaxTreeSkeleton ::
  Text
  -> [Sentence]
  -> SyntaxTree a
createSyntaxTreeSkeleton fn = fst . unzipper . toTop . foldl' acceptSentence (newSyntaxTree fn)

toTop :: Zipper a -> Zipper a
toTop = upWhile (\s -> case _nodeType $ focus s of
  RootNode -> False
  _ -> True
  )

acceptSentence ::
  Zipper a
  -> Sentence
  -> Zipper a
acceptSentence z sentence =
  detectChangeOfSourceFile z sentence -- &
  --parseDividingSentence


  -- if this sentence is in a different file to our current focus, then
  -- we want a new implicit super heading.
detectChangeOfSourceFile ::
  Zipper a
  -> Sentence
  -> Zipper a
detectChangeOfSourceFile z sentence = if traceShow (lastFile, sentenceFileOfOrigin sentence) (lastFile /= sentenceFileOfOrigin sentence)
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
  -> Zipper a
  -> Zipper a
makeNewHeadingNode ht hl hfn z =
  -- if the new heading level is lower than the current heading level, then
  -- we need to go *up* until we hit a higher level.
  upWhile (\z' -> getHeadingLevel z' > hl) z &
  graftNodeChild ((blankNode "" ("imp_heading_" <> fromMaybe "" hfn))
    { _nodeType = HeadingNode ht hl
    } )




