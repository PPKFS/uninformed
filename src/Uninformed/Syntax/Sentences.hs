module Uninformed.Syntax.Sentences where

import Uninformed.Syntax.SyntaxTree
import Uninformed.Words.Lexer
import Prelude hiding (Word)
import Uninformed.Words.Vocabulary
import qualified Data.Vector as V

type WordList = [Word]

breakIntoSentences ::
  SyntaxTree a
  -> WordList
  -> SyntaxTree a
breakIntoSentences sTree wl =
  let (newTree, remaining) = breakOffSentence sTree (trimText wl)
  in error ""

trimText ::
  WordList
  -> WordList
trimText = dropWhile (\w -> w ^. word == ParagraphBreak)

breakOffSentence ::
  SyntaxTree a
  -> WordList
  -> (SyntaxTree a, WordList)
breakOffSentence sTree wl =
  let enterTableMode = considerTableMode wl in
  (sTree, fst $ lookForSentenceBreak enterTableMode wl)

lookForSentenceBreak ::
  Bool
  -> WordList
  -> (WordList, WordList)
lookForSentenceBreak _ [] = ([], [])
lookForSentenceBreak inTableMode wl@(w:wr) = (map snd firstPart, otherStops)
  where (firstPart, (_, stopChar):rest) = break findFirstStop (zip wl (tail (w :| wr)))
        otherStops = takeWhile (findNextStops stopChar) rest
        findFirstStop (prev, curr) = matchWord
          (\case
            ParagraphBreak -> True
            Period -> True
            Semicolon -> True
            Colon -> considerColonDivision prev
            x -> considerQuotedPunctuation prev x

            ) curr

considerTableMode ::
  WordList
  -> Bool
considerTableMode [] = False
considerTableMode (w:_) = matchWord (== OrdinaryWord "table") w

-- the second sentence::make_node is actually OUTSIDE the looping, so it's about amending a full stop to
-- any floating words.
-- https://ganelson.github.io/inform/supervisor-module/6-st.html
-- is where the whole structural sentence preform is
-- get rid of all the pbreaks
-- maybe enter table mode
-- for each word:
-- look for a sentence break
-- count the stop words
-- go into table sentence mode if needed
