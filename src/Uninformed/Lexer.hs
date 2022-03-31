{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Uninformed.Lexer where

import Text.Megaparsec
import Solitude hiding (some)
import Data.Char (isAlphaNum)
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL
import qualified Data.Text as T
import Data.Text.Display
import qualified Data.Text.Lazy.Builder as TLB
import Data.List (singleton)


newtype SourceLocation = SourceLocation
  { _parseNodeSpan :: ((Int, SourcePos), (Int, SourcePos))
  } deriving newtype (Eq, Show, Ord)

newtype LexerState = LexerState
  { _lexLiteralMode :: Bool
  }

data Lexeme = Lexeme
  { _tokLoc :: SourceLocation
  , _tokType :: TokenType
  } deriving stock (Eq, Show)

instance Ord Lexeme where
  compare l1 l2 = _tokLoc l1 `compare` _tokLoc l2

data TokenType =
  Punctuation PunctuationToken
  | Comment Text
  | Whitespace (WhitespaceToken, Int)
  | Word Text deriving stock (Eq, Show)

instance Display TokenType where
  displayBuilder (Punctuation p) = displayBuilder p
  displayBuilder (Comment t) = "[" <> TLB.fromText t <> "]"
  displayBuilder (Whitespace (x, i)) = mconcat $ replicate i (TLB.fromText $ display x)
  displayBuilder (Word t) = TLB.fromText t

data PunctuationToken =
  StringBegin
  | StringEnd
  | TextSubstitutionBegin
  | TextSubstitutionEnd
  | Period
  | Comma
  | Dash
  | Semicolon
  | Colon
  | Underscore
  | ParenOpen
  | ParenClose deriving stock (Eq, Enum, Show)
instance Display PunctuationToken where
  displayBuilder d = case d of
    StringBegin -> "\""
    StringEnd -> "\""
    TextSubstitutionBegin -> "["
    TextSubstitutionEnd -> "]"
    Period -> "."
    Comma -> ","
    Dash -> "-"
    Semicolon -> ";"
    Colon -> ":"
    Underscore -> "_"
    ParenOpen -> "("
    ParenClose -> ")"


data WhitespaceToken = Newline | Space deriving stock (Eq, Show)

instance Display WhitespaceToken where
  displayBuilder Newline = "\n"
  displayBuilder Space = " "


makeLenses ''Lexeme
makeLenses ''LexerState
makePrisms ''PunctuationToken
makePrisms ''WhitespaceToken
makePrisms ''TokenType

punctuationToken :: 
  MonadState LexerState m
  => MonadParsec e Text m
  => m PunctuationToken
punctuationToken = choice
  [ beginOrEndLiteralMode
  , sym '(' ParenOpen
  , sym ')' ParenClose
  , textSubstitution
  , sym '.' Period
  , sym ',' Comma
  , sym '-' Dash
  , sym ';' Semicolon
  , sym ':' Colon
  , sym '_' Underscore
  ]

beginOrEndLiteralMode :: 
  MonadState LexerState m
  => MonadParsec e Text m
  => m PunctuationToken
beginOrEndLiteralMode = --if we're in literal mode, reading a speech mark takes us out
  (guardM inLiteralMode >> single '"' >> lexLiteralMode .= False >> return StringEnd)
  <|> (guardM (not <$> inLiteralMode) >> single '"' >> lexLiteralMode .= True >> return StringBegin)

textSubstitution ::
  MonadState LexerState m
  => MonadParsec e Text m 
  => m PunctuationToken
textSubstitution = do
  guardM inLiteralMode
  sym '[' TextSubstitutionBegin
  <|> sym ']' TextSubstitutionEnd

  
tabsToSpaces :: Int
tabsToSpaces = 4

whitespace ::
  MonadParsec e Text m
  => m TokenType
whitespace = do
  ws <- some (sym '\n' Newline)
    <|> mconcat <$> some ((singleton <$> sym ' ' Space) <|> (single '\t' $> replicate tabsToSpaces Space))
  case ws of
    [] -> error "some should never return an empty list.."
    xs@(x:_) -> return $ Whitespace (x, length xs)

data LexerError = LexerError deriving stock (Eq, Ord)

newtype LexerErrorBundle = LexerErrorBundle 
  { unLexerErrorBundleParseErrorBundle :: ParseErrorBundle Text LexerError }

lex ::
  Text
  -> Either LexerErrorBundle [Lexeme]
lex = first LexerErrorBundle <$> parse (evalStateT lexer (LexerState False)) ""
  where
    lexer = fst <$> manyTill_ (fmap (uncurry Lexeme) $ annotateToken $ choice
      [ Punctuation <$> punctuationToken
      , comment
      , whitespace
      , word
      ]) (annotateToken eof)

sym :: 
  (MonadParsec e Text m)
  => Char 
  -> a 
  -> m a
sym c t = t <$ single c

inLiteralMode ::
  (MonadState LexerState m)
  => m Bool
inLiteralMode = use lexLiteralMode

word :: 
  (MonadParsec e Text m)
  => m TokenType
word = Word <$> takeWhile1P Nothing isAlphaNum

comment :: 
  (MonadParsec e Text m)
  => m TokenType
comment = Comment <$> surroundM (single '[') (takeWhileP Nothing (/= ']')) (const $ single ']')

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
  return (SourceLocation ((b', b), (a', a)), r)

data LexemeStream = LexemeStream
  { _streamInput :: Text
  , _unStream :: [Lexeme]
  }

lsProxy :: Proxy LexemeStream
lsProxy = Proxy 

instance Stream LexemeStream where
  type Token LexemeStream = Lexeme
  type Tokens LexemeStream = [Lexeme]
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (LexemeStream _ []) = Nothing
  take1_ (LexemeStream str (t:ts)) = Just (t, LexemeStream (T.drop (tokensLength lsProxy (t:|[])) str) ts)
  takeN_ n (LexemeStream str s)
    | n <= 0    = Just ([], LexemeStream str s)
    | null s    = Nothing
    | otherwise = let (x, s') = splitAt n s
        in case nonEmpty x of
          Nothing -> Just (x, LexemeStream str s')
          Just nex -> Just (x, LexemeStream (T.drop (tokensLength lsProxy nex) str) s')
  takeWhile_ f (LexemeStream str s) =
    let (x, s') = DL.span f s
    in case nonEmpty x of
      Nothing -> (x, LexemeStream str s')
      Just nex -> (x, LexemeStream (T.drop (tokensLength lsProxy nex) str) s')

instance VisualStream LexemeStream where
  showTokens Proxy s = toString . unwords $ NE.toList $ fmap (display . _tokType) s
  tokensLength Proxy xs = sum (tokenLength <$> xs)

tokenLength :: 
  Lexeme 
  -> Int
tokenLength l = fst (lexemeStart l) - fst (lexemeEnd l)

lexemeStart :: 
  Lexeme 
  -> (Int, SourcePos)
lexemeStart (Lexeme (SourceLocation (a, _)) _) = a

lexemeEnd :: 
  Lexeme 
  -> (Int, SourcePos)
lexemeEnd (Lexeme (SourceLocation (_, a)) _) = a

instance TraversableStream LexemeStream where
  reachOffset o PosState{..} = (Just (prefix ++ restOfLine), PosState
        { pstateInput = LexemeStream postStr post
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        })
    where
      prefix = if sameLine then pstateLinePrefix ++ toString preLine else toString preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos = case post of
          [] -> pstateSourcePos
          (x:_) -> snd $ lexemeStart x
      (pre', post) = splitAt (o - pstateOffset) (_unStream pstateInput)
      (preStr, postStr) = T.splitAt tokensConsumed (_streamInput pstateInput)
      preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
      tokensConsumed =
        case nonEmpty pre' of
          Nothing -> 0
          Just nePre -> tokensLength (Proxy @LexemeStream) nePre
      restOfLine = takeWhile (/= '\n') $ toString postStr
