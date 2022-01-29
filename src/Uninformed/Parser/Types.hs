module Uninformed.Parser.Types
( Parser(..)
, ParseState(..)
, SnippetHandler(..)
, ParseErrorType(..)
, AsTextParser

, verbUsages
, allowNewlines
, inLiteralMode
, snippetHandler
, snippetStart
, snippetEnding
, snippetContext
, snippetFilename

, sentenceEndingPunctuation
, whitespaceCharacters
, newlineCharacters
, otherPunctuation

, UninformedParseError(..)

, ExtensionName(..)
, AST

, VerbConjugationTable(..)
, VerbUsage(..)
, VerbConjugation(..)
, Tense(..)
, Participle(..)
, Pronoun(..)
) where

import Relude
import qualified Data.Map.Strict as Map
import Text.Megaparsec (ParsecT, MonadParsec, Token, Tokens, SourcePos, PosState, Pos)
import Optics
import Chapelure.Types
import Data.Text.Display
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)

data ExtensionName = ExtensionName
  { _extensionTitle :: Text
  , _extensionAuthor :: Text
  } deriving stock (Eq, Show)

data Participle = Present | Past

-- he/she/it
data Pronoun = Singular | They

data Tense = Is | Was | HasBeen | HadBeen

data VerbConjugation = VerbConjugation
  { _conjugationPronoun :: Pronoun
  , _conjugationParticiple :: Maybe Participle
  , _conjugationText :: Text
  , _conjugationAdjectival :: Bool
  }

--todo: properly
type VerbConjugationTable = Map.Map Text (Set VerbUsage)

data VerbUsage = VerbUsage
  { _vuText :: Text -- ^ the name of this verb
  , _vuIsNegated :: Bool
  , _vuTense :: Tense
  , _vuMeaning :: ()
  , _vuUnderMainVerb :: ()
  , _vuSentenceIndex :: Int
  }

data ParseState = ParseState
  { _allowNewlines :: Bool
  , _inLiteralMode :: Bool
  , _snippetHandler :: SnippetHandler
  , _verbUsages :: Map.Map Text (Set VerbUsage)
  , _psDummy :: ()
  }

data SnippetHandler = SnippetHandler
  { _snippetStart :: PosState Text
  , _snippetEnding :: Parser ()
  , _snippetFilename :: Text
  , _snippetContext :: [Text]
  --, _snippetLastHighlightStart :: Int
 -- , _snippetHighlights :: [Source]
  }

data UninformedParseError = UninformedParseError
  { snippet :: Snippet
  , diagnosticHelp :: Text }
  deriving stock (Eq, Show)

data ParseErrorType = UnexpectedToken | MultipleParseErrors [ParseErrorType] | MissingQuoteEnd

prettyPrintList :: [Text] -> Text 
prettyPrintList [] = ""
prettyPrintList [x] = x
prettyPrintList [x, y] = x <> ", and " <> y
prettyPrintList (x:xs) = x <> ", " <> prettyPrintList xs

instance Display ParseErrorType where
  displayBuilder UnexpectedToken = "an unexpected token was found"
  displayBuilder (MultipleParseErrors xs) = fromText $ prettyPrintList $ map display xs
  displayBuilder MissingQuoteEnd = "an opening \" was found with but no closing \" was found to match it"
instance Ord UninformedParseError where
  compare (UninformedParseError (Snippet loc1 _ _ ) _)
    (UninformedParseError (Snippet loc2 _ _ ) _) = loc1 `compare` loc2

data AST = AST

sentenceEndingPunctuation :: [Char]
sentenceEndingPunctuation = ['.', ';', ':']

newlineCharacters :: [Char]
newlineCharacters = ['\r', '\n']

whitespaceCharacters :: [Char]
whitespaceCharacters = [' ', '\t']

otherPunctuation :: [Char]
otherPunctuation = ['(', ')', '"']

newtype Parser a =
  Parser { unParser :: StateT ParseState (ParsecT UninformedParseError Text IO) a }
    deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus,
    MonadIO, MonadState ParseState, MonadParsec UninformedParseError Text, MonadFail)

type AsTextParser e s m = (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)

makeLenses ''ParseState
makeLenses ''SnippetHandler