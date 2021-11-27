module Uninformed.Parser.Common
  ( Parser(..)
  , ParseState(..)
  , UninformedParseError(..)
  , AST
  , punctuationInHeading
  ) where

import Uninformed.Prelude
import Text.Megaparsec (ParsecT, MonadParsec, ShowErrorComponent (..), customFailure)

data ParseState = ParseState

data AST = AST

data UninformedParseError = PunctuationInHeading Char
  deriving stock (Ord, Eq)

instance ShowErrorComponent UninformedParseError where
  showErrorComponent (PunctuationInHeading p) = "Punctuation such as " <> [p] <> " is not allowed in headings. You can use ; and : in quoted text, but . nowhere."

punctuationInHeading 
  :: Char 
  -> Parser a
punctuationInHeading = customFailure . PunctuationInHeading
  
newtype Parser a =
  Parser { unParser :: StateT ParseState (ParsecT UninformedParseError Text IO) a }
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus,
    MonadIO, MonadState ParseState, MonadParsec UninformedParseError Text, MonadFail)

