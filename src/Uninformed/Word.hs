{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Word
  ( SourceLocation(..)
  , Whitespace(..)
  , Word(..)

  , displayWord
  , blankWord
  , matchWord
  , wordCount
  , quotedWordCount

  , VocabType(..)
  , PunctuationSet(..)
  , standardPunctuation
  , getPunctuation
  , lowerVocabType
  , _ParagraphBreak
  , _OrdinaryWord

  , isNumber

  , pattern Period
  , pattern Colon
  , pattern Semicolon
  , pattern CloseParenthesis
  , pattern OpenParenthesis
  ) where

import Uninformed.Prelude
import Data.Aeson ( FromJSON, ToJSON )
import Text.Megaparsec ( SourcePos )
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text.Lazy.Builder (fromText)
import Data.Char (isSpace)
import Numeric.Optics (decimal)

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

wordCount ::
  VocabType
  -> Int
wordCount s@(StringLit _) = quotedWordCount s
wordCount ParagraphBreak = 0
wordCount (OrdinaryWord w) = if T.all (`S.member` getPunctuation StandardPunctuation) w then 0 else 1
wordCount _ = 1

quotedWordCount :: VocabType -> Int
quotedWordCount (StringLit s) = length . filter (/= "") . T.split isSpace $ s
quotedWordCount _ = 0


data VocabType =
  I6 Text
  | StringLit Text
  | OrdinaryWord Text
  | StringSub Text
  | ParagraphBreak
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Hashable VocabType

pattern Period :: VocabType
pattern Period = OrdinaryWord "."

pattern Semicolon :: VocabType
pattern Semicolon = OrdinaryWord ";"

pattern Colon :: VocabType
pattern Colon = OrdinaryWord ":"

pattern OpenParenthesis :: VocabType
pattern OpenParenthesis = OrdinaryWord "("

pattern CloseParenthesis :: VocabType
pattern CloseParenthesis = OrdinaryWord ")"

data PunctuationSet = StandardPunctuation deriving stock (Eq, Show)

standardPunctuation :: Set Char
standardPunctuation = fromList ".,:;?!(){}[]"

getPunctuation :: PunctuationSet -> Set Char
getPunctuation StandardPunctuation = standardPunctuation

lowerVocabType :: VocabType -> VocabType
lowerVocabType = \case
  OrdinaryWord txt -> OrdinaryWord $ T.toLower txt
  x -> x

makePrisms ''VocabType

isNumber ::
  VocabType
  -> Bool
isNumber x = isJust $ x ^? _OrdinaryWord % to toString % decimal @Integer
