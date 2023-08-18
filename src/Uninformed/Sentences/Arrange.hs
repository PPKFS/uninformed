module Uninformed.Sentences.Arrange where

import Uninformed.Prelude
import Uninformed.Syntax.SyntaxTree
import Uninformed.Word
import Uninformed.Sentence

createSyntaxTreeSkeleton ::
  Text
  -> [Sentence]
  -> SyntaxTree a
createSyntaxTreeSkeleton fn = fst . unzipper . toTop . foldl' acceptSentence (newSyntaxTree fn)

toTop :: Zipper a -> Zipper a
toTop = upWhile (\s -> case nodeType $ focus s of
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
detectChangeOfSourceFile z sentence = if lastFile /= sentenceFileOfOrigin sentence
  then
    makeNewHeadingNode Implicit 0 (sentenceFileOfOrigin sentence) z
  else
    z
  where
    lastFile = focus z ^. #nodeLocation % #filename

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
    { nodeType = HeadingNode ht hl
    } )
