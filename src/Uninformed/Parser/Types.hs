{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Parser.Types
  ( ParseState(..)
  , SnippetHandler(..)
  , UninformedParseError(..)
  , ParseErrorType(..)
  , UninformedParser
  , Parser(..)

  , sentenceEndingPunctuation
  , newlineCharacters
  , whitespaceCharacters

  , allowNewlines
  , snippetHandler
  , snippetContext
  , snippetStart
  , snippetEnding
  , snippetFilename
  , snippetHighlightStart
  , verbUsages
  ) where

import qualified Data.Map.Strict as Map
import Text.Megaparsec 
import Optics
import Chapelure.Types
import Data.Text.Display
import Data.Text.Lazy.Builder (fromText)

import Solitude hiding (show)
import Uninformed.Lexer
import Uninformed.NewVerb.Types

data ParseState = ParseState
  { _allowNewlines :: Bool
  , _snippetHandler :: SnippetHandler
  , _verbUsages :: Map.Map Text (Set VerbUsage)
  }

data SnippetHandler = SnippetHandler
  { _snippetStart :: PosState Text
  , _snippetEnding :: Parser ()
  , _snippetFilename :: Text
  , _snippetContext :: [Text]
  , _snippetHighlightStart :: Maybe (Int, SourcePos)
 -- , _snippetHighlights :: [Source]
  }

data UninformedParseError = UninformedParseError
  { snippet :: Snippet
  , diagnosticHelp :: Text }
  deriving stock (Eq, Show)
  deriving Display
    via (ShowInstance UninformedParseError) 

data ParseErrorType = UnexpectedToken | MultipleParseErrors [ParseErrorType] | MissingQuoteEnd deriving stock (Eq, Ord)

instance Display ParseErrorType where
  displayBuilder UnexpectedToken = "an unexpected token was found"
  displayBuilder (MultipleParseErrors xs) = fromText $ prettyPrintList $ map display xs
  displayBuilder MissingQuoteEnd = "an opening \" was found with but no closing \" was found to match it"
instance Ord UninformedParseError where
  compare (UninformedParseError (Snippet loc1 _ _ ) _)
    (UninformedParseError (Snippet loc2 _ _ ) _) = loc1 `compare` loc2

instance ShowErrorComponent ParseErrorType where
  showErrorComponent = toString . display

instance ShowErrorComponent UninformedParseError where
  showErrorComponent = toString . display

sentenceEndingPunctuation :: [PunctuationToken]
sentenceEndingPunctuation = [Period, Semicolon, Colon]

newlineCharacters :: [Char]
newlineCharacters = ['\r', '\n']

whitespaceCharacters :: [Char]
whitespaceCharacters = [' ', '\t']

newtype Parser a =
  Parser { unParser :: StateT ParseState (ParsecT UninformedParseError [Lexeme] IO) a }
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus,
    MonadIO, MonadState ParseState, MonadParsec UninformedParseError [Lexeme], MonadFail)

type UninformedParser m = (MonadParsec UninformedParseError [Lexeme] m, MonadState ParseState m)


makeLenses ''ParseState
makeLenses ''SnippetHandler
