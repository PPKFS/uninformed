module Uninformed.Syntax.Sentences
  ( Sentence(..)
  , sentenceFileOfOrigin

  ) where

import Uninformed.Prelude
import Uninformed.Words.Lexer.Types

newtype Sentence = Sentence { unSentence :: NonEmpty Word }

sentenceFileOfOrigin ::
  Sentence
  -> Maybe Text
sentenceFileOfOrigin (Sentence (x :| _)) = x ^. #wordLocation % #sourceLocationFile