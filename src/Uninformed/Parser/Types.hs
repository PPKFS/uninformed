{-# LANGUAGE FlexibleInstances #-}
module Uninformed.Parser.Types where

import qualified Data.Map.Strict as Map
import Text.Megaparsec 
import Optics
import Chapelure.Types
import Data.Text.Display
import Data.Text.Lazy.Builder (fromText)

import Uninformed.Headings.Types
import Uninformed.Extensions.Types
import Uninformed.Prelude hiding (show)
import GHC.Show (show)

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
  deriving Display
    via (ShowInstance UninformedParseError) 

data ParseErrorType = UnexpectedToken | MultipleParseErrors [ParseErrorType] | MissingQuoteEnd deriving stock (Eq, Ord)

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

instance ShowErrorComponent ParseErrorType where
  showErrorComponent = toString . display

instance ShowErrorComponent UninformedParseError where
  showErrorComponent = toString . display

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

data ExprF r = 
  HeadingExpr Heading 
  | ExtensionExpr (ExtensionF r)
  | ExtensionHeaderExpr ExtensionHeader
  
  deriving stock (Eq, Show, Functor)

type ExprLoc = WithLoc ExprF
type ExprPlain = Fix ExprF

type Extension = ExtensionF ExprPlain

type Algebra f a = f a -> a 

cata :: Functor f => Algebra f a -> Fix f -> a 
cata f = f . fmap (cata f) . unFix

stripLoc :: ExprLoc -> ExprPlain
stripLoc = cata s where
  s (Compose (_, f)) = Fix f

instance Show ExprPlain where
  show f = cata show f

instance Eq ExprPlain where
  (==) (Fix a) (Fix b) = a == b

instance Show ExprLoc where
  show f = cata sAnno f where
    sAnno :: Compose ((,) SourceLocation) ExprF String -> String
    sAnno (Compose (r, g)) = show r <> show g


makeLenses ''ParseState
makeLenses ''SnippetHandler
