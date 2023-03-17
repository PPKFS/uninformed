{-# LANGUAGE StrictData #-}

module Uninformed.Words.TextFromFiles
  ( SourceFile(..)

  ) where

import Uninformed.Prelude

data SourceFile a = SourceFile
  { sourceFileName :: Maybe Text
  , sourceFileText :: Text
  , sourceFileWordCount :: Int
  , sourceFileRawWordCount :: Int
  , sourceFileQuotedWordCount :: Int
  , sourceFileData :: a
  } deriving stock (Eq, Show, Ord, Read, Generic, Functor)
