{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Lexer
  ( SourceLocation(..)
  ) where
import Optics.State.Operators
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (Word)
import Data.List (singleton)
import Data.Set (member)
import Data.Char

data SourceLocation = SourceLocation
  { _sourceSpan :: Maybe ((Int, SourcePos), (Int, SourcePos))
  , _wordNumber :: Int
  } deriving stock (Eq, Show, Ord)

data PunctuationSet = StandardPunctuation deriving stock (Eq, Show)

data LexerState = LexerState
  { _lexerPunctuationSet :: PunctuationSet
  }

data VocabEntry =
  I6 Text
  | StringLit Text
  | OrdinaryWord Text
  | StringSub Text  deriving stock (Eq, Show)

data Word = Word
  { _wordLocation :: SourceLocation
  , _word :: VocabEntry
  } deriving stock (Eq, Show)

instance Ord Word where
  compare l1 l2 = _wordLocation l1 `compare` _wordLocation l2

data Punctuation =
  StringBegin
  | StringEnd
  | SubStart
  | SubEnd
  | CommentBegin
  | CommentEnd
  | I6InclusionEnd
  | I6InclusionBegin
   deriving stock (Eq, Enum, Show, Bounded)

makeLenses ''LexerState

type Parser m = (MonadState LexerState m, MonadParsec Void Text m)

defaultLexerState :: LexerState
defaultLexerState = LexerState
  { _lexerPunctuationSet = StandardPunctuation
  }

lex ::
  Bool
  -> Text
  -> Either (ParseErrorBundle Text Void) [Word]
lex spl = parse (evalStateT lexer defaultLexerState) ""
  where
    lexer = mconcat <$> manyTill (do
      w <- parseWord spl
      -- the only cases where whitespace is required (breaking an ordinary word) is dealt with, but we may have more.
      void $ optional admireWhitespace
      pure $ w) eof

admireWhitespace :: m a0
admireWhitespace = error ""

parseWord ::
  Parser m
  => Bool
  -> m [Word]
parseWord spl = choice
  [ [] <$ comment
  , stringLit spl
  , singleton <$> i6Inclusion
  , singleton <$> ordinaryWord
  ]

standardPunctuation :: Set Char
standardPunctuation = fromList ".,:;?!(){}[]"

getPunctuation :: PunctuationSet -> Set Char
getPunctuation StandardPunctuation = standardPunctuation

ordinaryWord ::
  Parser m
  => m Word
ordinaryWord = (uncurry Word) <$$> annotateToken $ do
  w <- mconcat <$> manyTill (choice [
    surroundedPunctuation
    , toText . singleton <$> anySingle
    ]) (lookAhead standalonePunctuationOrSpace)
  admireWhitespace (OrdinaryWord w)
  return $ OrdinaryWord w

-- basically if we see something which would cause us to break the word *now*
-- it's all on a lookahead anyway so it's not a problem if we are greedy in what we look for
-- but this is the inverse of the inform parser; they check if they do *not* break whereas we
-- check for a breakage...this is therefore inverted
standalonePunctuationOrSpace ::
  Parser m
  => m ()
standalonePunctuationOrSpace = choice
  [ -- whitespace
    void $ satisfy isWhitespace
    -- some literal mode entering punctuation
  , void $ choice (map string' (map punctuationMap [CommentBegin, StringBegin, I6InclusionBegin]))
  , isPunctuationNoSlash
  ]

-- note that because / isn't punctuation, then reading :/ as in http:// means that only the colon
-- needs to be checked to ensure no, we do not wish to break. once we get to the //, it has no problems
-- just accepting it
isPunctuationNoSlash ::
  Parser m
  => m ()
isPunctuationNoSlash = do
  ps <- use lexerPunctuationSet
  void $ satisfy (`member` (getPunctuation ps))
  notFollowedBy (single '/')

isWhitespace :: Char -> Bool
isWhitespace = flip elem [' ', '\t', '\n', '\r']

-- if the /last/ character read was a digit then lc=1
  -- if it was a-z (not A-Z) then lc=2
  -- if the next character is a digit or a '-' then nc=1
  -- if it's a-z then nc=2
  -- if both are 1, then no space
  -- if it's a dot and both lc and nc are not 0, then no space
  -- otherwise space

-- if we fail this, which is very likely in the case of just reading word. And...
-- then we don't need to worry about losing our punctuation mark because we just continue
-- reading a regular word
surroundedPunctuation ::
  Parser m
  => m Text
surroundedPunctuation = try $ do
  a <- satisfy (\x -> isLower x || isDigit x) -- read either a digit or a lowercase letter
  ps <- use lexerPunctuationSet
  b <- satisfy (`member` (getPunctuation ps)) -- read the internal bit of the punctuation
  c <- satisfy (\x -> isLower x || isDigit x) -- once again read a lowercase or digit
  pure $ toText [a,b,c]


i6Inclusion ::
  Parser m
  => m Word
i6Inclusion = (uncurry Word) <$$> annotateToken $ do
  punctuation I6InclusionBegin
  I6 . toText <$>
    flip manyTill (punctuation I6InclusionEnd) anySingle

-- todo: this should handle whitespace better.
stringLiteralInternal ::
  Parser m
  => m (SourceLocation, VocabEntry)
stringLiteralInternal = annotateToken $ StringLit . toText <$> takeWhileP Nothing (\x -> not $ x `elem` ['"', '['])

stringLit ::
  Parser m
  => Bool
  -> m [Word]
stringLit spl = uncurry Word <$$> do
  punctuation StringBegin
  manyTill (
    if spl
      then ((annotateToken $ StringSub <$> subLiteral) <|> stringLiteralInternal)
      else stringLiteralInternal) (punctuation StringEnd)

subLiteral ::
  Parser m
  => m Text
subLiteral = do
  punctuation SubStart
  r <- toText <$> takeWhileP Nothing (/=']')
  punctuation SubEnd
  pure r

comment ::
  Parser m
  => m ()
comment = do
  punctuation CommentBegin
  flip manyTill (punctuation CommentEnd)
    ( (void comment)
      <|> (void anySingle) )
  pass

punctuation ::
  Parser m
  => Punctuation
  -> m ()
punctuation p = void $ string' (punctuationMap p)

punctuationMap :: Punctuation -> Text
punctuationMap = \case
  StringBegin -> "\""
  StringEnd -> "\""
  CommentBegin -> "["
  CommentEnd -> "]"
  I6InclusionBegin -> "(-"
  I6InclusionEnd -> "-)"
  SubStart -> "["
  SubEnd -> "]"

annotateToken ::
  (MonadParsec e Text m)
  => m a
  -> m (SourceLocation, a)
annotateToken p = do
  b <- getSourcePos
  b' <- getOffset
  r <- p
  a <- getSourcePos
  a' <- getOffset
  return (SourceLocation (Just ((b', b), (a', a))) 0, r)