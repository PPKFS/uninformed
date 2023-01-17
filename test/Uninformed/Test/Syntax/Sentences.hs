module Uninformed.Test.Syntax.Sentences
  ( parseExemplar
  , compareSentenceInfo
  ) where

import Uninformed.Prelude
import Uninformed.Words.Lexer
import Test.Tasty.HUnit
    ( (@=?), assertFailure, Assertion )
import qualified Data.Text as T
import Uninformed.Syntax.Sentences

-- | Parse an exemplar as given by the Inform syntax-test. return a list of sentence nodes
-- and the total number of sentences.
parseExemplar ::
  Text
  -> (Int, [Text])
parseExemplar t =
  let l = t &
        lines &
        map T.stripStart & -- clean whitespace
        dropWhile (/= "ROOT_NT") & -- drop the initial error lines
        drop 1 & --and also throw away the root node.
        mapMaybe toSentenceNode & -- make sentence nodes
        map (\x -> fromMaybe x ((\y -> T.stripSuffix "{level " y <|> T.stripSuffix "{heading" y) (T.dropEnd 2 x) ))
  in (length l, l)

sentencePrefixes :: [Text]
sentencePrefixes = ["SENTENCE_NT", "DIALOGUE_CUE_NT", "DIALOGUE_LINE_NT", "UNKNOWN_NT", "DIALOGUE_SELECTION_NT"
-- just ignore the actual subdivision of dialogue for now
   {- ,"DIALOGUE_CLAUSE_NT", "DIALOGUE_SPEECH_NT", "DIALOGUE_SPEAKER_NT"-} ]

toSentenceNode ::
  Text
  -> Maybe Text
toSentenceNode t
  | Just x <- listToMaybe (mapMaybe (`T.stripPrefix` t) sentencePrefixes) = Just $ T.dropEnd 1 x
  | Just _ <- T.stripPrefix "HEADING_NT" t >>= T.stripSuffix "{heading 0}" = Nothing
  | Just x <- T.stripPrefix "HEADING_NT" t = Just $ T.dropEnd 1 x
  | otherwise = Nothing

compareSentenceInfo :: (Int, [Text]) -> [Sentence] -> Assertion
compareSentenceInfo (numSentences, fullSentenceList) wl = if
  numSentences /= length wl
  then
    let zipL = zipWithPadding "" "" fullSentenceList (map (T.intercalate " " . map displayWord . toList . unSentence) wl) in
    assertFailure $ toString $ "Failed to match the right number of sentences, and instead we got \n" <>
      unlines (map show zipL) <> " exp" <> show numSentences <> " act" <> show (length wl)
  else
    numSentences @=? length wl