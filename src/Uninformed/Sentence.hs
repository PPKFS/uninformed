module Uninformed.Sentence
  ( Sentence(..)
  , sentenceFileOfOrigin

  ) where

import Uninformed.Prelude
import Uninformed.Word

newtype Sentence = Sentence { unSentence :: NonEmpty Token } deriving newtype (Show)

sentenceFileOfOrigin ::
  Sentence
  -> Maybe Text
sentenceFileOfOrigin (Sentence (x :| _)) = x ^. #location % #filename