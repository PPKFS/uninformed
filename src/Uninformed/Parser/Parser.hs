module Uninformed.Parser.Parser
  ( Parser(..)

  , withoutNewlines

  , headedSection
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
  , optionallyQuotedPhrase
  , optionallyInParens
  , word
  , rawQuotedStringLiteral


  , paragraphBreak

  , specifically
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
  let wrapper = maybe id (\x y -> withContext (x a) y) name
  wrapper (f a <$> bodyBlock)

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

-- | A phrase consists of repeated segments of text ended by some
-- construct, with optional errors built by `errorSnippet`.
phrase
  :: Parser Text -- ^ a phrase is some of these
  -> Text -- ^ intercalated with these
  -> [Parser b] -- ^ with errors if we run into these
  -> Parser a -- ^ ended by this
  -> Parser (Text, a)
phrase phraseComponent ic errs fin = do
  (pc, e) <- (do
    sequenceA_ errs
    phraseComponent) `someTill_` fin
  return (intercalate ic pc, e)

-- | because of how
optionallyQuotedPhrase
  :: Parser Text
  -> [Parser b]
  -> Parser a
  -> Parser (Text, a)
optionallyQuotedPhrase pc errs fin = do
  isQuoted <- optional $ specificallySymbol "\""
  --if we read a quotation, then we want to finish the phrase
  --by reading a quotation mark
  let finishQuote = whenJust isQuoted (const . void . specificallySymbol $ "\"")
  phrase pc " " errs (finishQuote >> fin)

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
word :: Parser Text
word = do
  w <- takeWhile1P Nothing
    (\x -> x `notElem`
      -- characters except ending punctuation or newlines or a space
      (sentenceEndingPunctuation <> newlineCharacters <> whitespaceCharacters <> otherPunctuation))
  --if we need to keep whitespace..
  _ <- consumeWhitespace
  return w

-- | read a string literal as a regular string.
rawQuotedStringLiteral
  :: Parser Text
rawQuotedStringLiteral = fst <$> inQuotes (phrase
  --either words, or some substitutions which we choose to ignore
  (word <|> ((\x -> mconcat $ ["["] <> x <> ["]"]) <$> inSquareBrackets (many word)))
  " "
  []
  (lookAhead (single '"')))

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
      _ <- anySingle `manyTill` (eos <|> eof)
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
      ls = lines snip

specifically
  :: Text
  -> Parser ()
specifically t = traverse_ specificWord (T.split isSpace t) where
  specificWord :: Text -> Parser ()
  specificWord w = do
    void (string' w >> consumeWhitespace)
    -- <|> errorSnippet word
    --  (\x -> "When looking for the word " <> t <>", instead '" <> x <> "' was found.")

specificallySymbol
  :: Text
  -> Parser ()
specificallySymbol w = void (string' w >> optional consumeWhitespace)

{-}
parseWordInternal
  :: Bool
  -> String
  -> Text
  -> Parser Text
parseWordInternal b ex t = do
  r <- (if b then string' else string) t
  consumeWhitespace ex
  return r




followedBy
  :: Parser b
  -> a
  -> Parser a
followedBy p a = do
  void p
  return a
parseWord
  :: Text
  -> Parser Text
parseWord = parseWordInternal False []

parseWordWithDelimiter'_
  :: [Char]
  -> Text
  -> Parser ()
parseWordWithDelimiter'_ = (void .) . parseWordInternal True

parseWord'
  :: Text
  -> Parser Text
parseWord' = parseWordInternal True []

parseWord'_
  :: Text
  -> Parser ()
parseWord'_ = void <$> parseWordInternal True []

parseWordOneOf'
  :: [Text]
  -> Parser Text
parseWordOneOf' l = choice (map (try . parseWord') l)

parseWords'
  :: Text
  -> Parser Text
parseWords' t = mconcat <$> traverse parseWord' (T.split isSpace t)



notSpaceOrPunctuation
  :: Char
  -> Bool
notSpaceOrPunctuation s = s `notElem` [' ', '\t', '.', ',', ';', ':']

isPunctuation
  :: Char
  -> Bool
isPunctuation s = s `elem` ['.', ',', ';', ':']

parsePhraseTillOneOf_
  :: [Text]
  -> Parser (Text, Text)
parsePhraseTillOneOf_ delims = parsePhrase_ (parseWordOneOf' delims)

parsePhraseTillWords
  :: Text
  -> Parser Text
parsePhraseTillWords t = parsePhrase (parseWords'_ t)

parsePhraseTillWord
  :: Text
  -> Parser Text
parsePhraseTillWord t = parsePhrase (parseWord'_ t)

parsePhraseTillAhead
  :: Parser a
  -> Parser Text
parsePhraseTillAhead p = parsePhrase (lookAhead p)

parseAnyWord :: Parser Text
parseAnyWord = do
  w <- takeWhile1P Nothing notSpaceOrPunctuation
  consumeWhitespace []
  return w

parsePhrase
  :: Parser a
  -> Parser Text
parsePhrase ending = T.intercalate " " <$> someTill parseAnyWord ending

parsePhrase_
  :: Parser a
  -> Parser (Text, a)
parsePhrase_ ending = first (T.intercalate " ") <$> someTill_ parseAnyWord ending


quotedStringWithoutNewlines :: Parser Text
quotedStringWithoutNewlines = wrap "\"" <$> quotedStringInternal

quotedStringInternal :: Parser Text
quotedStringInternal = inQuotes (takeWhileP Nothing (`notElem` ['"', '\n']))

quotedString :: Parser Text
quotedString = wrap "\"" <$> inQuotes (takeWhileP Nothing (/= '"'))

optionallyQuotedStringTill
  :: Parser a
  -> Parser Text
optionallyQuotedStringTill e = mconcat <$> someTill optionallyQuotedString e

optionallyQuotedString :: Parser Text
optionallyQuotedString = inQuotes quotedStringInternal <|> one <$> anySingle
-}