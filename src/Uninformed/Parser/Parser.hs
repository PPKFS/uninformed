module Uninformed.Parser.Parser 
  ( startSnippet
  , endSnippetAtParagraphBreak
  , withContext
  
  , specifically
  , specifically'
  , specificallySymbol
  , specificallySymbol'
  , withoutNewlines
  , phrase

  , headedSection
  , sentenceConsistingOf
  , verbPhrase
  , optionallyQuotedPhrase
  , rawStringLiteral
  , headerLikePhrase)
where

import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec hiding (unexpected)
import Uninformed.Parser.Types
import Optics hiding (pre)
import Optics.State.Operators
import qualified Data.Text as T
import Relude.Extra.Bifunctor (firstF)
import Uninformed.Parser.Errors
import Uninformed.Parser.Combinators


phrase ::
  [Parser ()] -- ^ with errors if we run into these
  -> Parser a -- ^ ended by this
  -> Parser (Text, a)
phrase errs fin = firstF unwords (extendedPhrase (word False) errs fin)

-- | A phrase consists of at least one word, with some possible errors, separated by whitespace.
extendedPhrase ::
  Parser component
  -> [Parser ()] -- ^ with errors if we run into these
  -> Parser end -- ^ ended by this
  -> Parser ([component], end)
extendedPhrase comp errs fin = do
  (pc, e) <- someTill_ (do
    wn <- use allowNewlines
    sequenceA_ (if not wn then unexpectedNewlineError : errs else errs)
    comp <|> unexpectedPhraseTokenError) fin
  return (pc, e)

sentenceConsistingOf ::
  Parser a
  -> Parser a
sentenceConsistingOf p = p <* (endSentence <|> expectedEndOfSentence)

-- | A verb phrase as described in 4/verb.
verbPhrase ::
  (header -> Text) -- ^ with this heading name
  -> Parser header -- ^ with this leading part
  -> [Text] -- ^ and containing any of these somewhere in the sentence
  -> Parser body -- ^ with this body
  -> (header -> body -> verb) -- ^ and this combiner
  -> Parser verb
verbPhrase f h xs p i = sentenceConsistingOf $ headedSection
  (Just f)
  (h <* choice (map sentenceContains xs))
  p
  i

optionallyQuotedPhrase
  :: [Parser ()] -- ^ with errors if we run into these
  -> Parser a -- ^ ended by this
  -> Parser (Text, a)
optionallyQuotedPhrase errs = optionallyQuotedWithEnding (phrase errs)

sentenceContains ::
  Text --if a sentence has this phrase somewhere in it as a standalone word
  -> Parser ()
sentenceContains t = if T.strip t == t then checkForContains (" " <> t <> " ") else checkForContains t
  where
    checkForContains t' = lookAhead $ do
      s <- manyTill anySingle (lookAhead $ satisfy $ \x -> x == '.')
      unless (t' `T.isInfixOf` toText s) (fail "")

rawStringLiteral ::
  Bool
  -> Parser Text
rawStringLiteral keepQuotes = do
  f <- specificallySymbol "\""
  inLiteralMode .= True
  s <- toText . fst <$> someTill_
    (satisfy (\x -> x `notElem` ('\"' : newlineCharacters)  ) <|> (' ' <$ unexpectedNewlineError)) (specificallySymbol "\"")
  inLiteralMode .= False
  return $ if keepQuotes then "\"" <> f <> s <> "\"" else s

headerLikePhrase :: Parser a -> Parser (Text, a)
headerLikePhrase p = first unwords <$> extendedPhrase
      ((rawStringLiteral True <|> word False) <?> "heading name stuff")
      [unexpectedPunctuationInHeading] -- no punctuation allowed
      p
      