module Uninformed.Syntax.Sentences
  ( Sentence(..)
  , sentenceFileOfOrigin

  ) where

import Uninformed.Words.Lexer.Types

newtype Sentence = Sentence (NonEmpty InformWord)

sentenceFileOfOrigin ::
  Sentence
  -> Maybe Text
sentenceFileOfOrigin (Sentence (x :| _)) = x ^. wordLocation % sourceLocationFile