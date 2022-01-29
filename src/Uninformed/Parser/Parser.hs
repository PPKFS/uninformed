module Uninformed.Parser.Parser
  ( Parser(..)

  , withoutNewlines

  , headedSection
  , ignoreHeader
  , inQuotes
  , inParentheses

  , errorSnippet
  , startSnippet
  , endSnippetAtSentence
  , endSnippetAtParagraphBreak
  , withContext
  , makeDiagnostic
  , buildSnippet

  , phrase
  , phraseUnexpectedTokenMsg
  , unexpectedNewlineMsg
  , amendLiteralModeMsg
  , extendedPhrase
  , optionallyQuotedPhrase
  , optionallyInParens
  , optionallyQuotedWithEnding
  , rawStringLiteral
  , word


  , paragraphBreak

  , specifically
  , specifically'
  , specificallySymbol
  , specificallySymbol'
  ) where

import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec hiding (unexpected)
import Text.Megaparsec.Char ( eol, string')
import Uninformed.Parser.Types
import Optics hiding (pre)
import Optics.State.Operators
import qualified Data.Text as T
import Data.Char (isSpace)
import Chapelure.Types
import qualified Data.Vector as Vec
import qualified Data.Vector.NonEmpty as NEVec
import Data.Text.Display
import Relude.Extra.Bifunctor (firstF)

-- | Mark the current input position as the start of a code snippet.
startSnippet
  :: Parser () -- ^ how to end this snippet
  -> Parser ()
startSnippet sEnd = do
  --update the posState machinery
  void getSourcePos
  ps <- statePosState <$> getParserState
  snippetHandler % snippetStart .= ps
  snippetHandler % snippetEnding .= sEnd

endSnippetAtParagraphBreak :: Parser ()
endSnippetAtParagraphBreak = lookAhead paragraphBreak

endSnippetAtSentence :: Parser ()
endSnippetAtSentence = void $ single '.'

withoutNewlines
  :: MonadState ParseState m
  => m a
  -> m a
withoutNewlines f = surroundM
  (use allowNewlines <* (allowNewlines .= False))
  f
  (allowNewlines .=)

headedSection
  :: Maybe (a -> Text) -- ^ the name of this section
  -> Parser a -- ^ the (backtrackable) leading segment
  -> Parser b -- ^ we've now committed to parsing a `b`
  -> (a -> b -> c) -- ^ a way to join the leading segment with the body
  -> Parser c -- ^ and finished with a `c`
headedSection name headerBlock bodyBlock f = do
  a <- try headerBlock
  maybe id (\x y -> withContext (x a) y) name (f a <$> bodyBlock)

ignoreHeader ::
  a
  -> b
  -> b
ignoreHeader _ = id

inQuotes
  :: AsTextParser e s m
  => m a
  -> m a
inQuotes = between (single '\"') (single '\"')

inParentheses
  :: AsTextParser e s m
  => m a
  -> m a
inParentheses = between (single '(') (single ')')

inSquareBrackets
  :: AsTextParser e s m
  => m a
  -> m a
inSquareBrackets = between (single '[') (single ']')

hspace1
  :: AsTextParser e s m
  => m Text
hspace1 = takeWhile1P Nothing (`elem` whitespaceCharacters)

hspace
  :: AsTextParser e s m
  => m Text
hspace = takeWhileP Nothing (`elem` whitespaceCharacters)

consumeWhitespace
  :: Parser Text
consumeWhitespace =
  --if it's immediately followed by a sentence ender
  "" <$ lookAhead (oneOf sentenceEndingPunctuation)
  --or it's followed by a paragraph break (again, don't consume)
  <|> "" <$ lookAhead paragraphBreak
  --or if we're currently treatingAsWhitespace some kind of punctuation
  -- <|> ("" <$ (use currentlyIgnoring >>= maybe mzero lookAhead))
  --take some amount of (horizontal) whitespace
  <|> hspace1
  <|> (do
    --we're allowing newlines
    guardM (use allowNewlines)
    --and we have optional space
    l1 <- hspace
    --then a newline
    e1 <- eol
    --then more optional space
    l2 <- hspace
    --and just to double check we didn't get a pbreak
    notFollowedBy eol
    return $ l1 <> e1 <> l2)

withContext
  :: Text
  -> Parser a
  -> Parser a
withContext t p = surroundM
  (snippetHandler % snippetContext %= (t :))
  p
  (const $ snippetHandler % snippetContext %= drop 1)

paragraphBreak :: Parser ()
paragraphBreak = do
  void $ try (hspace >> eol >> hspace)
  void eol
  --and then maybe a bunch more lines
  void $ many (hspace >> eol)

optionallyQuotedWithEnding ::
  (Parser end -> Parser (body, end)) -- given some kind of parser-with-ending gubbins
  -> Parser end -- and an ending gubbins
  -> Parser (body, end) --optionally quote the body
optionallyQuotedWithEnding f end = do
  isQuoted <- optional $ specificallySymbol "\""
  whenJust isQuoted (const $ inLiteralMode .= True) --enter literal mode; todo: check if this means we're doubling up?
  let finishQuote = whenJust isQuoted (const . void . specificallySymbol $ "\"")
  r <- f (finishQuote >> end)
  whenJust isQuoted (const $ inLiteralMode .= False)
  return r

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

unexpectedNewlineError :: Parser ()
unexpectedNewlineError = do
  ilm <- use inLiteralMode
  errorSnippet
    (do
      void $ takeWhile1P Nothing (`elem` newlineCharacters)
      use inLiteralMode)
    (if ilm then MultipleParseErrors [UnexpectedToken, MissingQuoteEnd] else UnexpectedToken)
    (\b -> unexpectedNewlineMsg <> (if b then amendLiteralModeMsg else ""))

  
amendLiteralModeMsg :: Text 
amendLiteralModeMsg = " We were in the middle of reading a quoted string; perhaps you forgot a \"?"

unexpectedNewlineMsg :: Text
unexpectedNewlineMsg = "When reading something, we found a newline in a phrase where newlines aren't allowed."

unexpectedPhraseTokenError :: Parser a
unexpectedPhraseTokenError = do
  errorSnippet
    anySingle
    UnexpectedToken
    (const phraseUnexpectedTokenMsg)
  fail "shouldnt get this far"

phraseUnexpectedTokenMsg :: Text
phraseUnexpectedTokenMsg = "When reading a phrase, found a character I wasn't expecting."
-- | A phrase consists of at least one word, with some possible errors, separated by whitespace.
phrase ::
  [Parser ()] -- ^ with errors if we run into these
  -> Parser a -- ^ ended by this
  -> Parser (Text, a)
phrase errs fin = firstF unwords (extendedPhrase (word False) errs fin)

optionallyQuotedPhrase
  :: [Parser ()] -- ^ with errors if we run into these
  -> Parser a -- ^ ended by this
  -> Parser (Text, a)
optionallyQuotedPhrase errs = optionallyQuotedWithEnding (phrase errs)

optionallyInParens
  :: Parser a
  -> Parser a
optionallyInParens p = do
  isQuoted <- optional $ specificallySymbol "("
  p <* whenJust isQuoted (const . void . specificallySymbol $ ")")

-- | a word is a string of characters except
-- sentence delimiting characters, newlines, or
-- quote characters.
-- then we consume any amount of whitespace.
word ::
  Bool
  -> Parser Text
word keepWhitespace = do
  let anyPunctuation = sentenceEndingPunctuation <> newlineCharacters <> whitespaceCharacters <> otherPunctuation
  w <- takeWhile1P Nothing
    (`notElem` anyPunctuation)
  --if we need to keep whitespace..
  ws <- consumeWhitespace <|> "" <$ lookAhead (satisfy (`elem` anyPunctuation))
  return $ w <> (if keepWhitespace then ws else mempty)

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


errorSnippet
  :: Parser a -- ^ the error that could be satisfied
  -> ParseErrorType
  -> (a -> Text) -- ^ the error message to show
  -> Parser ()
errorSnippet errParser eType errMsg = do
  i <- getOffset
  s <- getSourcePos
  p <- observing errParser
  case p of
    --if we errored on p, that's good
    Left _ -> pass
    --if we did parse p, then we should consume the rest of our snippet
    --and then report a parse error at the immediate index i
    --but also build up our 'proper' error
    Right x -> do
      sh <- use snippetHandler
      i2 <- getOffset
      curCxt <- buildSnippetContext eType
      let eos = _snippetEnding sh
          snipBeg = pstateOffset $ _snippetStart sh
      _ <- anySingle `manyTill` (eof <|> eos)
      i3 <- getOffset
      let len = i3-snipBeg
          snip = fst <$> takeN_ len (pstateInput $ _snippetStart sh)
          fp = _snippetFilename sh
          snippet = buildSnippet
            fp
            (fromMaybe "Somehow ran out of input.." snip)
            (unPos $ sourceLine s)
            (unPos $ sourceColumn s)
            [(1, i-snipBeg, i2-snipBeg, errMsg x)]
      parseError (FancyError i $ one $ ErrorCustom $ UninformedParseError snippet curCxt)

buildSnippetContext
  :: ParseErrorType
  -> Parser Text
buildSnippetContext eType = do
  let dis = display eType
  cxt <- use $ snippetHandler % snippetContext
  let builtCxt = intercalate ", " cxt
  return $ builtCxt <> ", " <> dis <> "."

makeDiagnostic
  :: Text
  -> Snippet
  -> Diagnostic
makeDiagnostic hlp snip = Diagnostic Nothing Error (Just hlp) Nothing
  (Just $ NEVec.singleton snip)

buildSnippet
  :: Text -- ^ the filename
  -> Text -- ^ the entire snippet's text
  -> Int -- ^ the start of the snippet's line
  -> Int -- ^ the start of the snippet's column
  -> [(Int, Int, Int, Text)] -- ^ list of highlight quadruples (line, startcol, endcol, text)
  -> Snippet
buildSnippet fp snip l c hs = Snippet
    { location = (fp, Line $ fromIntegral l, Column $ fromIntegral c)
    , highlights = NEVec.fromList $ map (\(l', s, e, m) ->
            Source{ label = Just m
                  , line = Line $ fromIntegral l'
                  , startColumn = Column (fromIntegral s)
                  , endColumn = Column (fromIntegral e)
                  }) hs
    , content = Vec.fromList ls
    }
    where
      ls = lines (T.strip snip)

specifically
  :: Text
  -> Parser Text
specifically t = mconcat <$> traverse specificWord (T.split isSpace t) where
  specificWord :: Text -> Parser Text
  specificWord w = string' w >> consumeWhitespace

specifically'
  :: Text
  -> Parser ()
specifically' t = void $ specifically t

specificallySymbol
  :: Text
  -> Parser Text
specificallySymbol w = fromMaybe "" <$> (string' w >> optional consumeWhitespace)

specificallySymbol'
  :: Text
  -> Parser ()
specificallySymbol' p = void $ specificallySymbol p