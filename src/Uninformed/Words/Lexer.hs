{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Words.Lexer
  ( SourceLocation(..)
  , Whitespace(..)
  , Word(..)
  , lex
  , isWhitespace
  , getPunctuation
  , PunctuationSet(..)
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char ( string' )
import Prelude hiding (many, some, Word)
import Data.List (singleton)
import Data.Set (member)
import Data.Char ( isSpace, isDigit, isPunctuation, isLower )
import Optics.State.Operators ( (.=) )
import Uninformed.Words.Vocabulary ( VocabType(..) )

data SourceLocation = SourceLocation
  { _sourceSpan :: Maybe ((Int, SourcePos), (Int, SourcePos))
  , _wordNumber :: Int
  } deriving stock (Eq, Show, Ord)

data PunctuationSet = StandardPunctuation deriving stock (Eq, Show)

data Whitespace =
  Space
  | Tab
  | TabIndent Int
  | Newline deriving stock (Eq, Show, Ord)

data LexerState = LexerState
  { _previousWhitespace :: Whitespace
  , _forceBreak :: Bool
  }

data Word = Word
  { _wordLocation :: SourceLocation
  , _word :: VocabType
  , _precedingWhitespace :: Whitespace
  } deriving stock (Eq, Show)

instance Ord Word where
  compare l1 l2 = _wordLocation l1 `compare` _wordLocation l2

makeLenses ''LexerState

isNewline :: Char -> Bool
isNewline x = x `elem` ['\n', '\DEL', '\r']

isHSpace :: Char -> Bool
isHSpace x = isSpace x && not (isNewline x)

isWhitespace :: Char -> Bool
isWhitespace = isSpace

nonWhitespace ::
 Parser m
  => m ()
nonWhitespace = satisfy (not . isWhitespace) >> pass

spaceOrTabOrNewline ::
  Parser m
  => m Char
spaceOrTabOrNewline = satisfy isWhitespace

type Parser m = (MonadState LexerState m, MonadParsec Void Text m)

defaultLexerState :: LexerState
defaultLexerState = LexerState
  { _previousWhitespace = Newline
  , _forceBreak = False
  }

lex ::
  Bool
  -> Text
  -> Either (ParseErrorBundle Text Void) [Word]
lex spl t = parse (evalStateT lexer defaultLexerState) "" ("\n" <> t  <> " \n\n\n\n ")
  where
    ps = StandardPunctuation
    lexer = do
      -- this is more of an inform quirk than anything..because it starts the lexer with a default of
      -- newline as the last seen space, then reading a single newline at the start of a file makes a paragraph break.
      -- ...yeah...
      pb <- maybe [] toPbreaks <$>
        ((Nothing <$ optional (admireWhitespace ps)) <|> optional (try pbreak))
      (\l -> pb ++ mconcat l) <$> manyTill (do
        w <- parseWord spl ps
        -- the only cases where whitespace is required (breaking an ordinary word) is dealt with, but we may have more.
        -- we don't want to double check the whitespace after we've broken for a punctuation mark because
        -- we don't want to add an excess space? Idek exactly what this does but it works
        (lookAhead (satisfy isPunctuation) >> pass) <|> (do
          mbWs <- optional (admireWhitespace ps)
          whenJust mbWs (previousWhitespace .=))
        pure w) eof

admireWhitespace ::
  Parser m
  => PunctuationSet
  -> m Whitespace
admireWhitespace ps = do
  Space <$ lookAhead (satisfy (`member` getPunctuation ps))
    -- we fake a newline if we see a pbreak
    <|> Newline <$ try (lookAhead pbreak)
    <|> (do
      -- either we record some amount of tabs and spaces
      -- or we record a pbreak upcoming, but we don't consume it here
      -- or we record a linebreak
      ws <- reverse <$> someTill spaceOrTabOrNewline (try $ lookAhead (nonWhitespace <|> void pbreak <|> eof))
      return $ findMostSignificantWhitespace $ break isNewline ws)

pbreak ::
  Parser m
  => m (Whitespace, Int, SourceLocation)
pbreak = do
  ps <- use previousWhitespace
  (s, i) <- annotateToken (do
    try (takeWhileP Nothing isHSpace >>
      satisfy isNewline <?> "first part of a paragraph break")
    n <- try $ someTill (do
      takeWhileP Nothing isHSpace
      satisfy isNewline <?> "second part of a paragraph break"
      takeWhileP Nothing isHSpace
      ) (lookAhead $ eof <|> nonWhitespace)
    pure $ length n)
  previousWhitespace .= Space
  pure (ps, i, s)

longestSequence :: (a -> Bool) -> [a] -> Int
longestSequence p x = ls x 0 0
  where ls [] mx cur = max mx cur
        ls (x':xs) mx cur
          | p x' = ls xs mx (cur+1)
          | otherwise = ls xs (max mx cur) 0

findMostSignificantWhitespace :: (String, String) -> Whitespace
findMostSignificantWhitespace (run, mbNewline) = case longestSequence (=='\t') run of
  0 -> if null mbNewline then Space else Newline
  x -> if null mbNewline then Tab else TabIndent x

parseWord ::
  Parser m
  => Bool
  -> PunctuationSet
  -> m [Word]
parseWord spl ps = choice
  [ [] <$ comment
  , toPbreaks <$> pbreak
  , stringLit spl
  , singleton <$> i6Inclusion
  , singleton <$> ordinaryWord ps
  ]

toPbreaks :: (Whitespace, Int, SourceLocation) -> [Word]
toPbreaks (w, n, srcLoc) = replicate n (Word srcLoc ParagraphBreak w)

standardPunctuation :: Set Char
standardPunctuation = fromList ".,:;?!(){}[]"

getPunctuation :: PunctuationSet -> Set Char
getPunctuation StandardPunctuation = standardPunctuation

ordinaryWord ::
  Parser m
  => PunctuationSet
  -> m Word
ordinaryWord ps = do
  pw <- use previousWhitespace
  (srcL, w) <- annotateToken $ do
    w <- mconcat <$> someTill (choice [
      surroundedPunctuation ps
      , toText . singleton <$> punctuationThenLetter ps
      , toText . singleton <$> anySingle
      ]) (guardM (use forceBreak) <|> lookAhead (standalonePunctuationOrSpace ps))
    ifM (use forceBreak)
      (forceBreak .= False >> previousWhitespace .= Space)
      (admireWhitespace ps >>= (previousWhitespace .=))
    return $ OrdinaryWord w
  pure (Word srcL w pw)

punctuationThenLetter ::
  Parser m
  => PunctuationSet
  -> m Char
punctuationThenLetter ps = try $ do
  p <- satisfy (`member` getPunctuation ps)
  lookAhead (satisfy (\x -> not (x `member` getPunctuation ps)))
  forceBreak .= True
  pure p

-- basically if we see something which would cause us to break the word *now*
-- it's all on a lookahead anyway so it's not a problem if we are greedy in what we look for
-- but this is the inverse of the inform parser; they check if they do *not* break whereas we
-- check for a breakage...this is therefore inverted
standalonePunctuationOrSpace ::
  Parser m
  => PunctuationSet
  -> m ()
standalonePunctuationOrSpace ps = choice
    [ -- whitespace
      void $ takeWhile1P Nothing (\x -> isWhitespace x || x == '[' || x == '"')
      -- some literal mode entering punctuation
    , void $ string' "(-"
    , isPunctuationNoSlash ps
    ]

-- note that because / isn't punctuation, then reading :/ as in http:// means that only the colon
-- needs to be checked to ensure no, we do not wish to break. once we get to the //, it has no problems
-- just accepting it
isPunctuationNoSlash ::
  Parser m
  => PunctuationSet
  -> m ()
isPunctuationNoSlash ps = do
  void $ satisfy (`member` getPunctuation ps)
  notFollowedBy (single '/')



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
  => PunctuationSet
  -> m Text
surroundedPunctuation ps = try $ do
  a <- satisfy (\x -> isLower x || isDigit x) -- read either a digit or a lowercase letter
  b <- satisfy (`member` getPunctuation ps) -- read the internal bit of the punctuation
  c <- satisfy (\x -> isLower x || isDigit x) -- once again read a lowercase or digit
  pure $ toText [a,b,c]


i6Inclusion ::
  Parser m
  => m Word
i6Inclusion = do
  (srcL, w) <- annotateToken $ do
    string' "(-"
    I6 . toText <$> manyTill anySingle (string' "-)")
  previousWhitespace .= Space
  pure (Word srcL w Space)

-- todo: this should handle whitespace better.
stringLiteralInternal ::
  Parser m
  => m (SourceLocation, VocabType)
stringLiteralInternal = annotateToken $ do
  t <- toText <$> manyTill (
    '\n' <$ try (takeWhileP Nothing isHSpace >> satisfy isNewline >> takeWhileP Nothing isHSpace) <|>
    ' ' <$ single '\160' <|>
    anySingle) (lookAhead $ single '"')
  pure $ StringLit t

stringLit ::
  Parser m
  => Bool
  -> m [Word]
stringLit spl = do
  pw <- use previousWhitespace
  ls <- annotateToken $ do
    single '"'
    manyTill (
      if spl
        then annotateToken (StringSub <$> (single '[' >> toText <$> manyTill anySingle (single ']')))
         <|> stringLiteralInternal
        else stringLiteralInternal) (single '"')
  previousWhitespace .= Space
  case ls of
    (s, []) -> pure [Word s (StringLit "") pw]
    (_, (sx, wx):xs) -> pure $ Word sx wx pw : map (\(sx', wx') -> Word sx' wx' Space) xs

comment ::
  Parser m
  => m ()
comment = do
  single '['
  manyTill ( void comment <|> void anySingle ) (single ']')
  pass

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