module Uninformed.Extensions.Types where

import Uninformed.Prelude hiding (some)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (hspace1)

data ExtensionF r = Extension deriving stock (Eq, Show, Functor)

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
