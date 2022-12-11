{-# LANGUAGE StrictData #-}

module Uninformed.Words.TextFromFiles
  ( SourceFile(..)
  , wordCount
  , quotedWordCount

  ) where

import Uninformed.Words.Vocabulary ( VocabType(..), PunctuationSet(..), getPunctuation )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Char (isSpace)

data SourceFile a = SourceFile
  { sourceFileName :: Maybe Text
  , sourceFileText :: Text
  , sourceFileWordCount :: Int
  , sourceFileRawWordCount :: Int
  , sourceFileQuotedWordCount :: Int
  , sourceFileData :: a
  } deriving stock (Eq, Show, Ord, Read, Generic, Functor)

wordCount :: VocabType -> Int
wordCount = \case
  (StringLit s) -> length $ filter (/= "") $ T.split isSpace s
  ParagraphBreak -> 0
  OrdinaryWord w -> if T.all (`S.member` getPunctuation StandardPunctuation) w then 0 else 1
  _otherwise -> 1

quotedWordCount :: VocabType -> Int
quotedWordCount = \case
  (StringLit s) -> length $ filter (/= "") $ T.split isSpace s
  _otherwise -> 0