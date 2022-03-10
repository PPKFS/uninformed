module Uninformed.Headings.Parser where

import Uninformed.Parser.Parser
import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec hiding (unexpected)
import Optics hiding ( noneOf )
import Uninformed.Parser.Types
import Uninformed.Headings.Types
import Uninformed.Extensions.Types
import Uninformed.Parser.Combinators
import Uninformed.Parser.Errors
import Uninformed.Parser.Expressions

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
  (headerLikePhrase ending)
  (\lvl (hnames, endings) -> (HeadingName lvl hnames, endings))

parseHeading :: Parser ExprLoc
parseHeading = annotateLocation $ do
  --withContext "In {heading}[a heading declaration]" $ do
  withContext "In a heading declaration" $ do
    startSnippet endSnippetAtParagraphBreak
    (hn, endings) <- parseHeadingName (try headingEndings) <?> "heading name"
    return . HeadingExpr $ Heading hn 0 (composel endings defaultHeadingInfo)

--consume any amount of extraneous header "stuff" that is solely there to be annoying
consumeHeaderFluff :: Parser ()
consumeHeaderFluff = void $
  takeWhileP Nothing (`elem` (headerFluff <> whitespaceCharacters))

-- seems like inform lets you put any number of endings together as long as they aren't separated by dashes
-- read some amount of excess whitespace or dash, then possibly some heading endings, then the end of line or input
headingEndings :: Parser [HeadingInfo -> HeadingInfo]
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
  
headingForUseWith :: 
  Bool
  -> ExtensionName
  -> HeadingInfo
  -> HeadingInfo
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
  