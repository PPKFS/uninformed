{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Lexer
  ( SourceLocation(..)
  ) where
import Optics.State.Operators

data SourceLocation = SourceLocation
  { _sourceSpan :: ((Int, Int), (Int, Int))
  , _sourceIndentLevel :: Int
  } deriving stock (Eq, Show, Ord, Read, Generic)

data PunctuationToken =
  DoubleQuote
  | Period
  | Comma
  | Dash
  | Semicolon
  | Colon
  | ParenOpen
  | ParenClose
  | BraceOpen
  | BraceClose
  | Bar
  | SingleQuote deriving stock (Eq, Enum, Show, Bounded)

type PhantomChar = Either Char Char

data KindOfWord = String | Comment | I6Inclusion deriving stock (Eq, Enum, Ord, Show, Bounded)

data LexerState = LexerState
  { _previousCharacter :: PhantomChar
  , _currentPunctuationMarks :: [Char]
  , _inLiteralMode :: Bool
  , _kindOfWord :: KindOfWord
  , _commentNesting :: Int
  , _currentWord :: [Char]
  , _currentLine :: [Either [Char] [Char]]
  , _inSoakUpWhitespaceMode :: Bool
  , _divideStringsAtTextSub :: Bool
  , _isScanningTextSubstitution :: Bool
  , _lineNo :: Int
  }

makeLenses ''LexerState

whenInLiteralMode ::
  MonadState LexerState m
  => m a
  -> m (Maybe a)
whenInLiteralMode f = ifM (use inLiteralMode) (Just <$> f) (pure Nothing)

feedChars ::
  MonadState LexerState m
  => [PhantomChar]
  -> m ()
feedChars = mapM_ feedChar

feedChar ::
  MonadState LexerState m
  => PhantomChar
  -> m ()
feedChar inp = do
  -- check if we are leaving literal mode
  newInput <- fromMaybe inp <$> whenInLiteralMode (do
    n1 <- considerLeaving inp
    -- mostly these will be no-ops
    ifM ((String ==) <$> use kindOfWord) (forceStringDivisionBegin n1 >>= soakUpWhitespace) (pure n1))

  -- whitespace outside literal mode ends any partly built word and need not be recorded
  ifM (pure (isWhitespace (either id id newInput)) &&^ not <$> use inLiteralMode)
    --then
    ( do
    admireWhitespaceTexture
    completeCurrentWord
    lineBreakOutsideLiteral
    )
    --otherwise record the current character as part of the word being built
    ( do
    amendCharacter newInput
    whenM (use isScanningTextSubstitution) (forceStringDivisionEnd newInput)
    when wordIsEmpty $ lookAtRecentWhitespace >> considerEntering
    )

-- words 3.26
feedTriplet ::
  MonadState LexerState m
  => (Char, Char, Char)
  -> m ()
feedTriplet trip@(bef, cur, aft) = do
  previousCharacter .= Right bef
  -- we insert spaces before/after punctuation and stuff
  x <- insertSpaceIfNecessary
  if x
  then feedChars [Left ' ', Right cur] >> maybeFeedAnotherSpace
  else feedChar (Right cur)
  when (cur == '\n') (lineNo %= (+1))

maybeFeedAnotherSpace ::
  MonadState LexerState m
  => m ()
maybeFeedAnotherSpace = unlessM (use inLiteralMode) (feedChar (Left ' '))

nothingToFalse ::
  Maybe Bool
  -> Bool
nothingToFalse = fromMaybe False



wordIsEmpty :: Bool
wordIsEmpty = error ""

considerEntering :: m ()
considerEntering = error ""

lookAtRecentWhitespace :: m a0
lookAtRecentWhitespace = error ""

lineBreakOutsideLiteral :: m ()
lineBreakOutsideLiteral = error ""

completeCurrentWord :: m a1
completeCurrentWord = error ""

admireWhitespaceTexture :: m a2
admireWhitespaceTexture = error ""

isWhitespace :: Char -> Bool
isWhitespace = error ""

soakUpWhitespace ::
  MonadState LexerState m
  => PhantomChar
  -> m PhantomChar
soakUpWhitespace e = fromMaybe e <$> runMaybeT (do
  guardM (use inSoakUpWhitespaceMode)
  pure e)

passthroughMaybe ::
  Functor m
  => a
  -> (MaybeT m) a
  -> m a
passthroughMaybe def f = fromMaybe def <$> runMaybeT f

forceStringDivisionBegin ::
  MonadState LexerState m
  => PhantomChar
  -> m PhantomChar
forceStringDivisionBegin c = passthroughMaybe c $ do
  guardM (use divideStringsAtTextSub)
  guard (c `isChar` SubBegin)
  feedChars $ map Left [getCharRep StringEnd, ' ', getCharRep SubSeparator, ' ']
  isScanningTextSubstitution .= True
  pure (Right ' ')

forceStringDivisionEnd ::
  MonadState LexerState m
  => PhantomChar
  -> m ()
forceStringDivisionEnd c = passthroughMaybe () $ do
  guardM (use divideStringsAtTextSub)
  guard (c `isChar` SubEnd)
  setLastCharacter (getCharRep SubSeparator)
  feedChars $ map Left [' ', getCharRep StringBegin]
  isScanningTextSubstitution .= True

setLastCharacter ::
  MonadState LexerState m
  => Char
  -> m ()
setLastCharacter c = currentWord %= (\cw -> c: drop 1 cw)

data CharType =
  CommentBegin
  | CommentEnd
  | StringBegin
  | StringEnd
  | I6EscapeEndPart1
  | I6EscapeEndPart2
  | SubBegin
  | SubSeparator
  | SubEnd

getCharRep :: CharType -> Char
getCharRep = \case
  CommentBegin -> '['
  CommentEnd -> ']'
  StringEnd -> '"'
  I6EscapeEndPart2 -> ')'
  I6EscapeEndPart1 -> '-'

isChar ::
  PhantomChar
  -> CharType
  -> Bool
isChar c ty = either id id c == getCharRep ty

considerLeaving ::
  MonadState LexerState m
  => PhantomChar
  -> m PhantomChar
considerLeaving c = passthroughMaybe c $ do
  guardM (use inLiteralMode)
  kindOf <- use kindOfWord
  prevChar <- use previousCharacter
  case kindOf of
    Comment
      | c `isChar` CommentBegin -> commentNesting %= (+1)
    Comment
      | c `isChar` CommentEnd -> do
        r <- commentNesting <%= (`subtract` 1)
        when (r == 0) $ inLiteralMode .= False
    String
      | c `isChar` StringEnd -> do
        inSoakUpWhitespaceMode .= False
        amendCharacter c
        inLiteralMode .= False
    I6Inclusion
      | c `isChar` I6EscapeEndPart2, prevChar `isChar` I6EscapeEndPart1 -> do
        --for some reason, the current code just ignores it?
        --but this screws up line counts, so I will not do that.
        amendCharacter c
        inLiteralMode .= False
    _ -> pass
  outLitMode <- (False==) <$> use inLiteralMode
  --well technically we're modifying the character rather than adding it.
  return $ if outLitMode then Right ' ' else c

amendCharacter ::
  MonadState LexerState m
  => PhantomChar
  -> m ()
amendCharacter (Right c) = currentWord %= (c:)
amendCharacter (Left _) = pass

insertSpaceIfNecessary :: m Bool
insertSpaceIfNecessary = error ""

data WhitespaceToken = Newline | Space deriving stock (Eq, Enum, Show, Bounded)
{-
instance Display WhitespaceToken where
  displayBuilder Newline = "\n"
  displayBuilder Space = " "


makeLenses ''Lexeme
makePrisms ''PunctuationToken
makePrisms ''WhitespaceToken
makePrisms ''TokenType

punctuationToken ::
  MonadParsec e Text m
  => m PunctuationToken
punctuationToken = choice
  [ sym '(' ParenOpen
  , sym ')' ParenClose
  , sym '.' Period
  , sym ',' Comma
  , sym '-' Dash
  , sym ';' Semicolon
  , sym ':' Colon
  , sym '{' BraceOpen
  , sym '}' BraceClose
  , sym '"' DoubleQuote
  , sym '|' Bar
  , sym '\'' SingleQuote
  ]

tabsToSpaces :: Int
tabsToSpaces = 4

newline ::
  MonadState LexerState m
  => MonadParsec e Text m
  => m [WhitespaceToken]
newline = do
  ws <- some (sym '\n' Newline)
  put (LexerState 0)
  return ws

hspace ::
  MonadState LexerState m
  => MonadParsec e Text m
  => m [WhitespaceToken]
hspace = do
  l <- indentLevel
  ws <- mconcat <$> some
    ((singleton <$> sym ' ' Space) <|> (single '\t' $> replicate tabsToSpaces Space))
  when (l == pos1) (put (LexerState $ length ws))
  return ws

whitespace ::
  MonadState LexerState m
  => MonadParsec e Text m
  => m TokenType
whitespace = do
  ws <- newline <|> hspace
  case ws of
    [] -> error "some should never return an empty list.."
    xs@(x:_) -> return $ Whitespace (x, length xs)

data LexerError = LexerError deriving stock (Eq, Ord, Show)

instance ShowErrorComponent LexerError where
  showErrorComponent = show
type LexerErrorBundle = ParseErrorBundle Text LexerError

lex ::
  Text
  -> Either LexerErrorBundle [Lexeme]
lex = parse (evalStateT lexer (LexerState 0)) ""
  where
    lexer = fst <$> manyTill_ (fmap (uncurry Lexeme) $ annotateToken $ choice
      [ Punctuation <$> punctuationToken
      , comment
      , whitespace
      , word
      ]) (annotateToken eof)

comment ::
  (MonadParsec e Text m)
  => m TokenType
comment = do
  single '#'
  Comment <$> (takeWhileP Nothing (/= '\n'))

sym ::
  (MonadParsec e Text m)
  => Char
  -> a
  -> m a
sym c t = t <$ single c

word ::
  (MonadParsec e Text m)
  => m TokenType
word = Word <$> takeWhile1P Nothing (\x -> isAlphaNum x || ('_' == x))

annotateToken ::
  MonadState LexerState m
  => MonadParsec e Text m
  => m a
  -> m (SourceLocation, a)
annotateToken p = do
  b <- getSourcePos
  b' <- getOffset
  i <- gets _curIndent
  r <- p
  a <- getSourcePos
  a' <- getOffset
  return (SourceLocation ((b', b), (a', a)) i, r)
-}