{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}

module Uninformed.Words.Lexer.Types
  ( SourceLocation(..)
  , Whitespace(..)
  , Word(..)
  , LexerInput(..)

  , displayWord
  , blankWord
  , matchWord

  ) where

import Uninformed.Prelude
import Text.Megaparsec ( SourcePos )
import Uninformed.Words.Vocabulary ( VocabType(..) )
import Data.Text.Lazy.Builder (fromText)
import Data.Aeson

data LexerInput = LexerInput
  { divideLiteralsAtSubstitutions :: Bool
  , sourceFilename :: Maybe Text
  , textStream :: Text
  }

data SourceLocation = SourceLocation
  { sourceLocationFile :: Maybe Text
  , sourceSpan :: Maybe ((Int, SourcePos), (Int, SourcePos))
  , wordNumber :: Int
  } deriving stock (Eq, Show, Ord, Read, Generic)

data Whitespace =
  Space
  | Tab
  | TabIndent Int
  | Newline
  deriving stock (Eq, Show, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Word = Word
  { wordLocation :: SourceLocation
  , word :: VocabType
  , precedingWhitespace :: Whitespace
  } deriving stock (Eq, Show, Read, Generic)

blankWord :: Word
blankWord = Word
  { wordLocation = SourceLocation Nothing Nothing (-1)
  , word = ParagraphBreak
  , precedingWhitespace = Newline
  }

instance Display Word where
  displayBuilder = fromText . displayWord

displayWord ::
  Word
  -> Text
displayWord Word{word} = case word of
  I6 txt -> "(-"<>txt<>"-)"
  StringLit txt -> "\""<>txt<>"\""
  OrdinaryWord txt -> txt
  StringSub txt -> "["<>txt<>"]"
  ParagraphBreak -> "\n\n"

instance Ord Word where
  compare :: Word -> Word -> Ordering
  compare l1 l2 = wordLocation l1 `compare` wordLocation l2

matchWord ::
  (VocabType -> Bool)
  -> Word
  -> Bool
matchWord f Word{word} = f word