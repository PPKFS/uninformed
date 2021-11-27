module Uninformed.Parser.Headings
  ( Heading(..)
  , HeadingLevel(..)
  , HeadingName(..)
  , InPlaceOf(..)
  , UseWith(..)
  , ForRelease(..)
  , parseHeading
  , makeHeading
  , makeHeading'
  ) where

import Uninformed.Parser.Common
import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec
import Uninformed.Parser.Extensions
import Text.Megaparsec.Char (eol, string', space1, string)
import qualified Data.Text as T
import Optics hiding ( noneOf )
import Text.Megaparsec.Char.Lexer (lexeme, symbol', symbol)
import Data.Char (isSpace)

data HeadingLevel = File | Volume | Book | Part | Chapter | Section
  deriving stock (Eq, Enum, Ord, Show, Generic)

data HeadingName = HeadingName
  { _headingLevel :: HeadingLevel
  , _headingText :: Text
  }
  deriving stock (Eq, Show)

data ForRelease = ForReleaseOnly | NotForRelease
  deriving stock (Eq, Show)

data Heading = Heading
  { _headingName :: HeadingName
  , _headingIndentation :: Int -- ?
  , _headingForRelease :: Maybe ForRelease
  , _headingIsIndexed :: Bool
  , _headingOmitMaterial :: Bool
  , _headingUseWith :: Maybe UseWith
  , _headingInPlaceOf :: Maybe InPlaceOf
  } deriving stock (Eq, Show)

data InPlaceOf = InPlaceOf HeadingName Extension deriving stock (Eq, Show)

data UseWith = UseWith Extension | UseWithout Extension deriving stock (Eq, Show)
data Title

makeLenses ''Heading
makeLenses ''HeadingName

makeHeading
  :: HeadingName
  -> Heading
makeHeading hn = Heading hn 0 Nothing True False Nothing Nothing

makeHeading'
  :: HeadingLevel
  -> Text
  -> Heading
makeHeading' h t = makeHeading (HeadingName h t)

eoi :: Parser ()
eoi = void eol <|> eof

{-
All are case-insensitive
<heading> ::= <paragraph-break> <heading-level> <heading-name> {<heading-end> [space | tab]} <paragraph-break>
<heading-level> ::= Part | Section | Chapter | Book | Volume
<heading-text> ::= { letter | digit | <quoted-heading-name> | space | tab | anything except .}+
<heading-name> ::= { <heading-text> | anything except ; : . "}
<heading-end> ::= "unindexed" | "not for release" | "for release only"
<quoted-heading-name> ::= "<heading-text>"
-}

parseHeadingName :: Parser a -> Parser (HeadingName, a)
parseHeadingName ending = do
  lvl <- parseHeadingLevel <?> "heading level"
  (hnames, endings) <- someTill_ (
    quotedStringWithoutNewlines <|>
    (one <$> noneOf  [';', ':', '.']) <|>
    (lookAhead anySingle >>= punctuationInHeading)) (try ending) <?> "heading name"
  return (HeadingName lvl $ mconcat hnames, endings)

parseHeading :: Parser Heading
parseHeading = do
  (hn, endings) <- parseHeadingName (try headingEndings)
  return $ composel endings $ makeHeading hn

-- seems like inform lets you put any number of endings together as long as they aren't separated by dashes
-- read some amount of excess whitespace or dash, then possibly some heading endings, then the end of line or input
headingEndings :: Parser [Heading -> Heading]
headingEndings = do
  -- consume any excess whitespace/dashesion {_extensionName = "Questions", _extens
  hspaceOrDashOrComma
  endings <- sepBy (
    (headingIsIndexed .~ False) <$ string' "unindexed" <|>
    (headingForRelease ?~ NotForRelease) <$ string' "not for release" <|>
    (headingForRelease ?~ ForReleaseOnly) <$ string' "for release only" <|>
    try ((\(h, e) -> headingInPlaceOf ?~ InPlaceOf h e) <$> inPlaceOf) <|>
    try (headingForUseWith False <$> forUse "without") <|>
    try (headingForUseWith True <$> forUse "with")
    ) hspaceOrDashOrComma1
  eoi
  return endings

parseWordInternal
  :: [Char]
  -> Bool
  -> Text
  -> Parser Text
parseWordInternal ex b t = try $ do
  r <- (if b then string' else string) t
  void $ takeWhile1P Nothing (\s -> s `elem` ([' ', '\t'] <> ex))
  return r

parseWord
  :: Text
  -> Parser Text
parseWord = parseWordInternal [] False

parseWord'
  :: Text
  -> Parser Text
parseWord' = parseWordInternal [] True

parseWord'_
  :: Text
  -> Parser ()
parseWord'_ = void <$> parseWordInternal [] True

parseWords'
  :: Text
  -> Parser Text
parseWords' t = mconcat <$> traverse parseWord' (T.split isSpace t)

parseWords'_
  :: Text
  -> Parser ()
parseWords'_ t = traverse_ parseWord'_ (T.split isSpace t)

forUse
  :: Text
  -> Parser Extension
forUse t = inParentheses $ do
  parseWords'_ ("for use " <> t)
  parseExtensionName

parseExtensionName :: Parser Extension
parseExtensionName = do
  --then we read a title
  t' <- optionallyQuotedStringTill (parseWord'_ " by")
  a <- optionallyQuotedStringTill (lookAhead $ single ')')
  return (Extension (toText t')  (toText a))

optionallyQuotedStringTill
  :: Parser a
  -> Parser Text
optionallyQuotedStringTill e = mconcat <$> someTill optionallyQuotedString e

optionallyQuotedString :: Parser Text
optionallyQuotedString = inQuotes stringWithoutNewlines <|> (one <$> anySingle)

optionallyQuoted
  :: Parser a
  -> Parser a
optionallyQuoted p = try $ inQuotes p <|> p

inPlaceOf :: Parser (HeadingName, Extension)
inPlaceOf = inParentheses $ do
  parseWords'_ "in place of"
  hn <- (fst <$> try (single '\"' >> parseHeadingName (parseWord' "\" in"))) 
     <|> fst <$> parseHeadingName (parseWord' " in")
  ex <- parseExtensionName
  return (hn, ex)

headingForUseWith
  :: Bool
  -> Extension
  -> Heading
  -> Heading
headingForUseWith True e = headingUseWith ?~ UseWith e
headingForUseWith False e = headingUseWith ?~ UseWithout e

inQuotes
  :: MonadParsec e s m
  => Token s ~ Char
  => m a
  -> m a
inQuotes = between (single '\"') (single '\"')

inParentheses
  :: MonadParsec e s m
  => Token s ~ Char
  => m a
  -> m a
inParentheses = between (single '(') (single ')')

quotedStringWithoutNewlines :: Parser Text
quotedStringWithoutNewlines = wrap "\"" <$> inQuotes stringWithoutNewlines

stringWithoutNewlines :: Parser Text
stringWithoutNewlines = takeWhileP Nothing (`notElem` ['"', '\n'])

hspaceOrDashOrComma :: Parser ()
hspaceOrDashOrComma = void $ optional hspaceOrDashOrComma1

hspaceOrDashOrComma1 :: Parser ()
hspaceOrDashOrComma1 = void $ takeWhile1P Nothing (`elem` [' ', '\t', '-', ','])

parseHeadingLevel :: Parser HeadingLevel
parseHeadingLevel =
  (Volume <$ symbolSpace "volume" ) <|>
  (Part <$ symbolSpace "part") <|>
  (Chapter <$ symbolSpace "chapter") <|>
  (Book <$ symbolSpace "book") <|>
  (Section <$ symbolSpace "section")
    where
      symbolSpace :: Text -> Parser Text
      symbolSpace = symbol' (hspaceOrDashOrComma1 <?> "heading name")

paragraphBreak :: Parser ()
paragraphBreak = eol >> eol >> pass

parseTitle :: Parser Title
parseTitle = error "Not implemented"