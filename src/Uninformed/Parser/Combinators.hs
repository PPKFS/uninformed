module Uninformed.Parser.Combinators
  ( specifically
  , specifically'
  , specificallySymbol
  , specificallySymbol'
  , withContext
  , headedSection

  , ignoreHeader
  , inQuotes
  , inParentheses
  , inSquareBrackets
  , hspace 
  , hspace1
  , paragraphBreak
  
  , withoutNewlines
  , withNewlines
  , endSentence
  , optionallyQuotedWithEnding
  , optionallyInParens) where


import Solitude hiding (some, many)
import Uninformed.Parser.Types
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Uninformed.Lexer hiding (whitespace)
import Data.Text.Display
import Text.Megaparsec (MonadParsec, satisfy, choice, lookAhead, try)


---
-- primitive combinators
---

single :: 
  MonadParsec UninformedParseError [Lexeme] m
  => TokenType
  -> m ()
single ttok = void $ satisfy (\Lexeme{_tokType} -> _tokType == ttok)

specifically :: 
  Text
  -> Parser Text
specifically t = mconcat <$> traverse specificWord (T.split isSpace t) where
  specificWord :: Text -> Parser Text
  specificWord w = do
    single (Word w)
    ws <- whitespace
    return $ w <> ws   

-- | Whitespace is one of the following:
-- if it's immediately followed by a sentence ending, then 'nothing' is acceptable
-- it's followed by a paragraph break (again, don't consume)
-- take some amount of (horizontal) whitespace
-- or finally, we're allowing newlines and we have optional space, then a newline,then more optional space
-- and just to double check we didn't get a paragraph break
whitespace :: Parser Text
whitespace = choice
  [ "" <$ lookAhead (oneOfPunctuation sentenceEndingPunctuation)
  , "" <$ try (lookAhead paragraphBreak)
  , hspace1
  , oneSingleLineBreak
  ]

hspace1 :: UninformedParser m
  => m Text
hspace1 = takeWhile1P Nothing (`elem` whitespaceCharacters)

oneOfPunctuation :: [PunctuationToken ] -> Parser PunctuationToken 
oneOfPunctuation ls = satisfy (isPunctuationToken . _tokType)

oneSingleLineBreak :: Parser Text
oneSingleLineBreak = do
  guardM (use allowNewlines)
  l1 <- hspace
  e1 <- newline (Just 1)
  l2 <- hspace
  notFollowedBy (newline Nothing)
  return $ l1 <> e1 <> l2
specifically' :: 
  Text
  -> Parser ()
specifically' t = void $ specifically t

specificallySymbol :: 
  Text
  -> Parser Text
specificallySymbol w = fromMaybe "" <$> (string' w >> optional consumeWhitespace)

specificallySymbol' :: 
  Text
  -> Parser ()
specificallySymbol' p = void $ specificallySymbol p

withContext
  :: Text
  -> Parser a
  -> Parser a
withContext t p = surroundM
  (snippetHandler % snippetContext %= (t :))
  p
  (const $ snippetHandler % snippetContext %= drop 1)
  
headedSection :: 
  Maybe (a -> Text) -- ^ the name of this section
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
  :: UninformedParser m
  => m a
  -> m a
inQuotes = between (single '\"') (single '\"')

inParentheses
  :: UninformedParser m
  => m a
  -> m a
inParentheses = between (single '(') (single ')')

inSquareBrackets
  :: UninformedParser m
  => m a
  -> m a
inSquareBrackets = between (single '[') (single ']')




paragraphBreak :: Parser ()
paragraphBreak = do
  void $ try (hspace >> eol >> hspace)
  void eol
  --and then maybe a bunch more lines
  void $ many (hspace >> eol)



withoutNewlines :: 
  Parser a
  -> Parser a
withoutNewlines f = surroundM
  (use allowNewlines <* (allowNewlines .= False))
  f
  (allowNewlines .=)

withNewlines :: 
  Parser a
  -> Parser a
withNewlines f = surroundM
  (use allowNewlines <* (allowNewlines .= True))
  f
  (allowNewlines .=)

endSentence :: Parser ()
endSentence = do
  specificallySymbol' "."

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

optionallyInParens
  :: Parser a
  -> Parser a
optionallyInParens p = do
  isQuoted <- optional $ specificallySymbol "("
  p <* whenJust isQuoted (const . void . specificallySymbol $ ")")