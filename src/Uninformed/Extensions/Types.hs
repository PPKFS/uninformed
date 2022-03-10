module Uninformed.Extensions.Types where

import Uninformed.Prelude hiding (some)

data Extension r = Extension deriving stock (Eq, Show, Functor)

data ExtensionName = ExtensionName
  { _extensionTitle :: Text
  , _extensionAuthor :: Text
  } deriving stock (Eq, Show)

data ExtensionVersion = ExtensionVersion
  { _extensionRelease :: Int
  , _extensionSerial :: Maybe Int
  } deriving stock (Eq, Show)

data ExtensionHeader = ExtensionHeader
  { _extensionName :: ExtensionName
  , _extensionVersion :: Maybe ExtensionVersion
  } deriving stock (Eq, Show)
