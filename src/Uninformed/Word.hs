{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Word
  ( Word(..)
  , Token(..)
  , Whitespace(..)
  , standardPunctuation
  , quotedWordCount
  , wordCount
  , matchWord
  , blankToken
  , isNumber
  , lowerWord
  , displayWord

  , pattern Period
  , pattern Semicolon
  , pattern Colon
  , pattern OpenParenthesis
  , pattern CloseParenthesis
  ) where

import Uninformed.Prelude
import Data.Aeson
import Uninformed.SourceFile
import Data.Text.Lazy.Builder (fromText)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char (isSpace)
import Numeric.Optics (decimal)

data Whitespace =
  Space
  | Tab
  | TabIndent Int
  | Newline
  deriving stock (Eq, Show, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Token = Token
  { location :: SourceLocation
  , word :: Word
  , precedingWhitespace :: Whitespace
  , vocabularyEntry :: Int
  } deriving stock (Eq, Show, Read, Generic)

blankToken :: Token
blankToken = Token
  { location = SourceLocation Nothing Nothing (-1)
  , word = ParagraphBreak
  , precedingWhitespace = Newline
  , vocabularyEntry = -1
  }

data Word =
  I6 Text
  | StringLit Text
  | OrdinaryWord Text
  | StringSub Text
  | ParagraphBreak
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Hashable Word

instance Display Token where
  displayBuilder = fromText . displayWord . word

displayWord ::
  Word
  -> Text
displayWord = \case
  I6 txt -> "(-"<>txt<>"-)"
  StringLit txt -> "\""<>txt<>"\""
  OrdinaryWord txt -> txt
  StringSub txt -> "["<>txt<>"]"
  ParagraphBreak -> "\n\n"

instance Ord Token where
  compare :: Token -> Token -> Ordering
  compare l1 l2 = location l1 `compare` location l2

matchWord ::
  (Word -> Bool)
  -> Token
  -> Bool
matchWord f Token{word} = f word

wordCount ::
  Word
  -> Int
wordCount s@(StringLit _) = quotedWordCount s
wordCount ParagraphBreak = 0
wordCount (OrdinaryWord w) = if T.all (`S.member` standardPunctuation) w then 0 else 1
wordCount _ = 1

quotedWordCount :: Word -> Int
quotedWordCount (StringLit s) = length . filter (/= "") . T.split isSpace $ s
quotedWordCount _ = 0

standardPunctuation :: Set Char
standardPunctuation = fromList ".,:;?!(){}[]"

lowerWord :: Word -> Word
lowerWord = \case
  OrdinaryWord txt -> OrdinaryWord $ T.toLower txt
  x -> x

makePrisms ''Word

isNumber ::
  Word
  -> Bool
isNumber x = isJust $ x ^? _OrdinaryWord % to toString % decimal @Integer

pattern Period :: Word
pattern Period = OrdinaryWord "."

pattern Semicolon :: Word
pattern Semicolon = OrdinaryWord ";"

pattern Colon :: Word
pattern Colon = OrdinaryWord ":"

pattern OpenParenthesis :: Word
pattern OpenParenthesis = OrdinaryWord "("

pattern CloseParenthesis :: Word
pattern CloseParenthesis = OrdinaryWord ")"
