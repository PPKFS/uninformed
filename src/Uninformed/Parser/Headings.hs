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
  , unexpectedPunctuationInHeadingMsg
  ) where

import Uninformed.Parser.Parser
import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec hiding (unexpected)
import Optics hiding ( noneOf )
import Uninformed.Parser.Types
import Data.Text.Display

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
  deriving Display via (ShowInstance Heading)

data InPlaceOf = InPlaceOf HeadingName ExtensionName deriving stock (Eq, Show)

data UseWith = UseWith ExtensionName | UseWithout ExtensionName deriving stock (Eq, Show)

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

{-
All are case-insensitive
<heading> ::= <paragraph-break> <heading-level> <heading-name> {<heading-end> [space | tab]} <paragraph-break>
<heading-level> ::= Part | Section | Chapter | Book | Volume
<heading-text> ::= { letter | digit | <quoted-heading-name> | space | tab | anything except .}+
<heading-name> ::= { <heading-text> | anything except ; : . "}
<heading-end> ::= "unindexed" | "not for release" | "for release only"
<quoted-heading-name> ::= "<heading-text>"
-}

parseHeadingName
  :: Parser a -- ^ the ending of the header
  -> Parser (HeadingName, a)
parseHeadingName ending = withoutNewlines $ headedSection
  Nothing -- we don't know what the section is called yet
  parseHeadingLevel -- we know we're parsing a header after seeing the first word
  (do
    extendedPhrase
      ((rawStringLiteral True <|> word False) <?> "heading name stuff")
      [unexpectedPunctuationInHeading] -- no punctuation allowed
      ending)
  (\lvl (hnames, endings) -> (HeadingName lvl (unwords hnames), endings))

unexpectedPunctuationInHeading :: Parser ()
unexpectedPunctuationInHeading = errorSnippet
  (do
    guardM (not <$> use inLiteralMode) --if we're in literal mode, we'll allow it.
    takeWhile1P Nothing (`elem` sentenceEndingPunctuation))
  UnexpectedToken
  (const unexpectedPunctuationInHeadingMsg)

unexpectedPunctuationInHeadingMsg :: Text
unexpectedPunctuationInHeadingMsg = "Some sentence-ending punctuation was found (;, :, or .). To use punctuation in a heading, wrap it in double-quotes."

parseHeading :: Parser Heading
parseHeading = do
  --withContext "In {heading}[a heading declaration]" $ do
  withContext "In a heading declaration" $ do
    startSnippet endSnippetAtParagraphBreak
    (hn, endings) <- parseHeadingName (try headingEndings) <?> "heading name"
    return $ composel endings $ makeHeading hn

headerFluff :: [Char]
headerFluff = [',', '-', '~']

--consume any amount of extraneous header "stuff" that is solely there to be annoying
consumeHeaderFluff :: Parser ()
consumeHeaderFluff = void $
  takeWhileP Nothing (`elem` (headerFluff <> whitespaceCharacters ))

-- seems like inform lets you put any number of endings together as long as they aren't separated by dashes
-- read some amount of excess whitespace or dash, then possibly some heading endings, then the end of line or input
headingEndings :: Parser [Heading -> Heading]
headingEndings = do
  consumeHeaderFluff
  r <- sepBy (
    (headingIsIndexed .~ False) <$ specifically "unindexed" <|>
    (headingForRelease ?~ NotForRelease) <$ specifically "not for release" <|>
    (headingForRelease ?~ ForReleaseOnly) <$ specifically "for release only" <|>
    (\(h, e) -> headingInPlaceOf ?~ InPlaceOf h e) <$> inPlaceOf <|>
    headingForUseWith False <$> forUse "without" <|>
    headingForUseWith True <$> forUse "with"
    ) consumeHeaderFluff
  paragraphBreak
  return r

forUse
  :: Text
  -> Parser ExtensionName
forUse t = headedSection
  (Just $ const "For use with...")
  (specificallySymbol' "(" >> specifically' ("for use " <> t))
  (parseExtensionName <* specificallySymbol' ")")
  ignoreHeader

parseExtensionName :: Parser ExtensionName
parseExtensionName = withoutNewlines $ do
  t' <- fst <$> phrase
    [unexpectedPunctuationInHeading]
    (specifically "by")
  a <- fst <$> phrase
    [unexpectedPunctuationInHeading]
    (lookAhead $ single ')')
  return (ExtensionName t' a)

inPlaceOf :: Parser (HeadingName, ExtensionName)
inPlaceOf = headedSection
  (Just $ const "In place of...")
  (specificallySymbol' "(" >> specifically' "in place of")
  ((do --then either parse a heading wrapped in quotes, or just a heading name
    hn <- fst <$> optionallyQuotedWithEnding parseHeadingName (specifically "in")
    ex <- parseExtensionName
    return (hn, ex)) <* specificallySymbol' ")")
  ignoreHeader
  
headingForUseWith
  :: Bool
  -> ExtensionName
  -> Heading
  -> Heading
headingForUseWith True e = headingUseWith ?~ UseWith e
headingForUseWith False e = headingUseWith ?~ UseWithout e

parseHeadingLevel :: Parser HeadingLevel
parseHeadingLevel = withoutNewlines $ do
  hl <- Volume <$ specifically "volume"
    <|> Part <$ specifically "part"
    <|> Chapter <$ specifically "chapter"
    <|> Book <$ specifically "book"
    <|> Section <$ specifically "section"
  consumeHeaderFluff
  return hl