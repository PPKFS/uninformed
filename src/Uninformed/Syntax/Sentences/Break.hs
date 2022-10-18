module Uninformed.Syntax.Sentences.Break
  ( breakIntoSentences

  ) where

import Uninformed.Words.Lexer
import Prelude hiding ( (|>) )
import Uninformed.Words.Vocabulary
import qualified Data.Text as T
import Data.Char (isUpper, isPunctuation)
import Data.Sequence ( (|>) )
import Uninformed.Words.Lexer.Types ( matchWord, precedingWhitespace, word )
import Uninformed.Syntax.Sentences

breakIntoSentences ::
  [InformWord]
  -> [Sentence]
breakIntoSentences wl = catMaybes . toList $ go wl empty
  where
    go [] s = s
    go wl' s = let (ns, r) = breakOffSentence (trimText wl') in go r (s |> ns)
    trimText = dropWhile ((ParagraphBreak ==) . view word)
    breakOffSentence = considerTableMode >>= lookForSentenceBreak

lookForSentenceBreak ::
  Bool
  -> [InformWord]
  -> (Maybe Sentence, [InformWord])
lookForSentenceBreak _ [] = (Nothing, [])
lookForSentenceBreak inTableMode wl@(_:wr) =
  (case viaNonEmpty Sentence (map (view _2) firstPart) of
    -- this happens when the stopchar is part of the actual word.
    -- for example, a sentence consisting of a single quoted string is a 1 word sentence that contains its own
    -- stopchar.
    Nothing -> Just (Sentence (stopChar :| []))
    Just x -> Just x, dropWhile findNextStops (map (view _2) rest))
  where
    (firstPart, stopAndRemainder) = break findFirstStop (zip3 (blankWord:wl) wl (snoc wr blankWord))
    (stopChar, rest) = maybe (blankWord, []) (first (view _2)) $ uncons stopAndRemainder
    findFirstStop (prev, curr, lookA) = matchWord
      (\case
        ParagraphBreak -> True
        Period -> True
        Semicolon -> True
        Colon -> considerColonDivision prev lookA
        _ -> considerQuotedPunctuation (_word curr) (_word lookA)
        ) curr
    findNextStops = matchWord
      (\w' -> case (_word stopChar, w') of
        (Colon, ParagraphBreak) -> error "colon at end of paragraph"
        (_, ParagraphBreak) -> True
        --let's ignore dialogue mode for now.
        (Colon, Period) -> error "colon at end of sentence"
        (Semicolon, Period) -> error "semicolon at end of sentence"
        (_, Period) -> True
        (Colon, Semicolon) -> error "semicolon after colon"
        (Period, Semicolon) -> error "semicolon after period"
        (_, Semicolon) -> True
        _ -> False
      )
    considerQuotedPunctuation curr next = not inTableMode && endsInPunctuation curr && isUppercaseWord next
    isUppercaseWord (OrdinaryWord w') = maybe False (isUpper . fst) $ T.uncons w'
    isUppercaseWord _ = False
    endsInPunctuation (StringLit w') = maybe False (isPunctuation . snd) $ T.unsnoc w'
    endsInPunctuation _ = False

-- we also ignore this DIVIDE_AT_COLON_SYNTAX_CALLBACK nonsense.
-- inform checks if we do not break file boundaries, which we also ignore for now.
-- basically this is a long winded check for 4:50pm (e.g.)
considerColonDivision ::
  InformWord
  -> InformWord
  -> Bool
considerColonDivision prev lookA = not $
  matchWord isNumber prev
  && matchWord isNumber lookA
  && (lookA ^. precedingWhitespace) `elem` [Space, Tab]

considerTableMode ::
  [InformWord]
  -> Bool
considerTableMode [] = False
considerTableMode (w:_) = matchWord (== OrdinaryWord "table") w
