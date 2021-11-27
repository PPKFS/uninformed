module Uninformed.Parser.Extensions
  ( Extension(..)    
  ) where

import Uninformed.Prelude

data Extension = Extension
  { _extensionName :: Text
  , _extensionAuthor :: Text
  } deriving stock (Eq, Show)