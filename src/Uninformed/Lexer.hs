{- |
Copyright: (c) 2021 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <avery@chordify.net>

See README for more info
-}

module Uninformed.Lexer where
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Relude
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, alphaNumChar, hspace1, eol)

data LexerSettings = LexerSettings
  { _stringDelimiter :: Char
  , _textSubstitutionBegin :: Char
  , _textSubstitutionEnd :: Char
  , _textSubstitutionSeparator :: Char
  , _commentBegin :: Char
  , _commentEnd :: Char
  , _codeInsertionBegin :: Char
  , _codeInsertionEnd :: Char
  , _paragraphBreak :: Char
  , _newlineInString :: Char
  , _punctuation :: [Char]

  , _divideStringsAtSubstitutions :: Bool
  , _treatSlashAsPunctuation :: Bool
  }

defaultLexerSettings :: LexerSettings
defaultLexerSettings = LexerSettings
  { _stringDelimiter = '"'
  , _textSubstitutionBegin = '['
  , _textSubstitutionEnd = ']'
  , _textSubstitutionSeparator = ','
  , _commentBegin = '['
  , _commentEnd = ']'
  , _codeInsertionBegin = '{'
  , _codeInsertionEnd = '}'
  , _paragraphBreak = '|'
  , _newlineInString = '\f'
  , _punctuation = ['.', ',', ':', ';', '?', '!', '(', ')', '{', '}', '[', ']']

  , _divideStringsAtSubstitutions = False
  , _treatSlashAsPunctuation = False
  }

data Punctuation = Period | Comma | Colon | Semicolon | QuestionMark | ExclamationMark | LBracket | RBracket | LBrace | RBrace
  | LParen | RParen

newtype SourceInput = SourceInput Text
type Lexer = ReaderT LexerSettings (Parsec Void Text)

type Lexeme = (LexemeType, SourcePos)

data LexemeType =
  OrdinaryWord Text
  | PunctuationToken Char
  | LiteralStart
  | LiteralEnd
  | SubstitutionStart
  | SubstitutionEnd
  | ParagraphBreak
  | SourceFileChange
  | EndOfFile
  | BlankLexeme
  | TimeLexeme Int Int
  deriving stock (Show, Ord, Eq)

lexSp :: Lexer ()
lexSp = L.space hspace1 empty empty

-- Consume whitespace following a lexeme, but record
-- its endpoint as being before the whitespace.
lexeme :: Lexer a -> Lexer (a, SourcePos)
lexeme parser = (,) <$> parser <*> (getSourcePos <* lexSp)

sourceToLexemes :: Lexer [Lexeme]
sourceToLexemes = do
  lexSp
  r <- manyTill (L.lexeme lexSp $
    stringLiteral
    <|> regularWord
    <|> comment
    <|> punctuation
    <|> try paragraphBreak
    <|> lineBreak) eof
  e <- getSourcePos
  return $ mconcat r <> [(EndOfFile, e)]

-- we just ignore linebreaks
lineBreak :: Lexer [a]
lineBreak = do
  _ <- L.lexeme lexSp ([] <$ eol)
  return []

paragraphBreak :: Lexer [Lexeme]
paragraphBreak = do
  p <- getSourcePos
  v <- L.lexeme lexSp (ParagraphBreak <$ eol)
  v'<- MP.some $ L.lexeme lexSp (ParagraphBreak <$ eol)
  case v' of
    [] -> return []
    (x:_) -> return [(x, p)]

comment :: ReaderT LexerSettings (Parsec Void Text) [Lexeme]
comment = do
  ls <- ask
  let sl = _commentBegin ls
  let se = _commentEnd ls
  _ <- L.lexeme lexSp (char sl) *> manyTill L.charLiteral (L.lexeme lexSp $ char se)
  return []

stringLiteral :: Lexer [Lexeme]
stringLiteral = do
  ls <- ask
  let sl = _stringDelimiter ls
  st <- lexeme (char sl)
  r <- manyTill stringLitInternal (lexeme (char sl))
  en <- getSourcePos
  return $ [(LiteralStart, snd st)] <> mconcat r <> [(LiteralEnd, en)]

stringLitInternal :: Lexer [Lexeme]
stringLitInternal = L.lexeme lexSp $
  regularWord
  <|> textSubstitution
  <|> punctuation
  <|> paragraphBreak

textSubstitution :: Lexer [Lexeme]
textSubstitution = do
  ls <- ask
  let ss = _textSubstitutionBegin ls
  let se = _textSubstitutionEnd ls
  st <- lexeme (char ss)
  r <- manyTill substitutionInternal (lexeme (char se))
  en <- getSourcePos
  return $ [(SubstitutionStart, snd st)] <> mconcat r <> [(SubstitutionEnd, en)]

substitutionInternal :: Lexer [Lexeme]
substitutionInternal = L.lexeme lexSp $ regularWord <|> punctuation

punctuation :: Lexer [Lexeme]
punctuation = do
  ls <- ask
  pos <- getSourcePos
  p <- PunctuationToken <$> satisfy (\x -> x `elem` _punctuation ls)
  return [(p, pos)]

regularWord :: Lexer [Lexeme]
regularWord = do
  pos <- getSourcePos
  w <- OrdinaryWord <$> (T.pack <$> MP.some alphaNumChar)
  return [(w, pos)]