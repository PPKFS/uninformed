{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Words.Lexer.Types
  ( SourceLocation(..)
  , Whitespace(..)
  , InformWord(..)
  , LexerInput(..)

  , displayWord
  , blankWord
  , matchWord

  ) where

import Text.Megaparsec ( SourcePos )
import Uninformed.Words.Vocabulary ( VocabType(..) )
import Data.Text.Lazy.Builder (fromText)

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
  | Newline deriving stock (Eq, Show, Ord, Read, Generic)

data InformWord = InformWord
  { wordLocation :: SourceLocation
  , word :: VocabType
  , precedingWhitespace :: Whitespace
  } deriving stock (Eq, Show, Read, Generic)

blankWord :: InformWord
blankWord = InformWord
  { wordLocation = SourceLocation Nothing Nothing (-1)
  , word = ParagraphBreak
  , precedingWhitespace = Newline
  }

instance Display InformWord where
  displayBuilder = fromText . displayWord

displayWord ::
  InformWord
  -> Text
displayWord InformWord{word} = case word of
  I6 txt -> "(-"<>txt<>"-)"
  StringLit txt -> "\""<>txt<>"\""
  OrdinaryWord txt -> txt
  StringSub txt -> "["<>txt<>"]"
  ParagraphBreak -> "\n\n"

instance Ord InformWord where
  compare :: InformWord -> InformWord -> Ordering
  compare l1 l2 = wordLocation l1 `compare` wordLocation l2

matchWord ::
  (VocabType -> Bool)
  -> InformWord
  -> Bool
matchWord f InformWord{word} = f word

makeLenses ''InformWord
makeLenses ''SourceLocation