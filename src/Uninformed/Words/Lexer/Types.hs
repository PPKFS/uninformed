{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Words.Lexer.Types
  ( SourceLocation(..)
  , Whitespace(..)
  , InformWord(..)

  , displayWord
  , blankWord
  , matchWord

  , precedingWhitespace
  , word

  ) where

import Text.Megaparsec (SourcePos)
import Uninformed.Words.Vocabulary

data SourceLocation = SourceLocation
  { _sourceSpan :: Maybe ((Int, SourcePos), (Int, SourcePos))
  , _wordNumber :: Int
  } deriving stock (Eq, Show, Ord, Read, Generic)

data Whitespace =
  Space
  | Tab
  | TabIndent Int
  | Newline deriving stock (Eq, Show, Ord, Read, Generic)

data InformWord = InformWord
  { _wordLocation :: SourceLocation
  , _word :: VocabType
  , _precedingWhitespace :: Whitespace
  } deriving stock (Eq, Show, Read, Generic)

blankWord :: InformWord
blankWord = InformWord
  { _wordLocation = SourceLocation Nothing (-1)
  , _word = ParagraphBreak
  , _precedingWhitespace = Newline
  }

displayWord ::
  InformWord
  -> Text
displayWord InformWord{_word} = case _word of
  I6 txt -> "(-"<>txt<>"-)"
  StringLit txt -> "\""<>txt<>"\""
  OrdinaryWord txt -> txt
  StringSub txt -> "["<>txt<>"]"
  ParagraphBreak -> "\n\n"

instance Ord InformWord where
  compare :: InformWord -> InformWord -> Ordering
  compare l1 l2 = _wordLocation l1 `compare` _wordLocation l2

matchWord ::
  (VocabType -> Bool)
  -> InformWord
  -> Bool
matchWord f InformWord{_word} = f _word

makeLenses ''InformWord