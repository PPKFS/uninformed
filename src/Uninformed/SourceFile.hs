{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Uninformed.SourceFile
  ( SourceFile(..)
  , SourceLocation(..)
  ) where

import Uninformed.Prelude
import Text.Megaparsec.Pos (SourcePos)

data SourceFile a = SourceFile
  { filename :: Maybe Text
  , rawText :: Text
  , fileWordCount :: Int
  , fileRawWordCount :: Int
  , fileQuotedWordCount :: Int
  , contents :: a
  } deriving stock (Eq, Show, Ord, Read, Generic, Functor)

data SourceLocation = SourceLocation
  { filename :: Maybe Text
  , span :: Maybe ((Int, SourcePos), (Int, SourcePos))
  , wordNumber :: Int
  } deriving stock (Eq, Show, Ord, Read, Generic)

makeFieldLabelsNoPrefix ''SourceLocation
makeFieldLabelsNoPrefix ''SourceFile