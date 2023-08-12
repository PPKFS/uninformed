{-# OPTIONS_GHC -Wno-orphans #-}

module Uninformed.Lexer.Combinators
  ( LexerState(..)
  , Parser
  , nonWhitespace
  , admireWhitespace
  , pbreak
  , findMostSignificantWhitespace
  , initialNewlines
  , makeParagraphBreaks
  , comment
  , annotateToken
  , punctuationThenLetter
  , standalonePunctuationOrSpace
  , betterSurround
  ) where

import Uninformed.Prelude hiding ( gets, get, modify, state )

import Control.Monad.State ( gets )
import Data.Char ( isSpace, isDigit, isLower )
import Data.Set ( member )

import Optics.State ( use )
import Optics.State.Operators ((.=))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char ( string' )

import Uninformed.Word
import Uninformed.SourceFile

import qualified Data.Set as Set
import Error.Diagnose.Compat.Megaparsec
import Error.Diagnose

type LexerPipelineStage i o = i -> Either StructuredError o
type Parser m = (MonadState LexerState m, MonadParsec Void Text m)

data LexerState = LexerState
  { previousWhitespace :: Whitespace
  , forceBreak :: Bool
  , currentFilename :: Maybe Text
  } deriving stock (Show, Ord, Eq, Generic)


type StructuredError = Diagnostic Text

instance HasHints Void msg where
  hints _ = mempty

-- | match anything that isn't whitespace
nonWhitespace ::
 Parser m
  => m Char
nonWhitespace = satisfy (not . isSpace)

admireWhitespace ::
  Parser m
  => m Whitespace
admireWhitespace = do
  Space <$ lookAhead (satisfy (`member` standardPunctuation))
    -- we fake a newline if we see a pbreak
    <|> Newline <$ try (lookAhead pbreak)
    <|> (do
      -- either we record some amount of tabs and spaces
      -- or we record a pbreak upcoming, but we don't consume it here
      -- or we record a linebreak
      ws <- reverse <$> someTill (satisfy isSpace) (try $ lookAhead (void nonWhitespace <|> void pbreak <|> eof))
      return $ findMostSignificantWhitespace $ break isNewline ws)

pbreak ::
  Parser m
  => m (Whitespace, Int, SourceLocation)
pbreak = do
  ps <- use #previousWhitespace
  (s, (i, n)) <- annotateToken (do
    try (takeWhileP Nothing isHspace >>
      satisfy isNewline)
    n <- try $ someTill (do
      takeWhileP Nothing isHspace
      satisfy isNewline
      takeWhileP Nothing isHspace
      ) (lookAhead $ eof <|> void nonWhitespace)
    pure $ (length n, toString $ mconcat n))
  -- if we ended on a bunch of tabs, we need to check indentation
  -- for the dialogue beats.
  #previousWhitespace .= (case longestSpan (=='\t') n of
    0 -> Space
    x -> TabIndent x)
  pure (ps, i, s)

findMostSignificantWhitespace :: (String, String) -> Whitespace
findMostSignificantWhitespace (run, mbNewline) = case longestSpan (=='\t') run of
  0 -> if null mbNewline then Space else Newline
  x -> if null mbNewline then Tab else TabIndent x

initialNewlines ::
  Parser m
  => m [Token]
initialNewlines = do
  mbPbreaks <- makeParagraphBreaks <$$> ((Nothing <$ optional admireWhitespace) <|> optional (try pbreak))
  pure $ fromMaybe [] mbPbreaks

makeParagraphBreaks ::
  (Whitespace, Int, SourceLocation)
  -> [Token]
makeParagraphBreaks (w, n, srcLoc) = replicate n (Token srcLoc ParagraphBreak w (-1))

punctuationThenLetter ::
  Parser m
  => m Char
punctuationThenLetter = try $ do
  p <- satisfy (`member` standardPunctuation)
  lookAhead (satisfy (\x -> not (x `member` standardPunctuation)))
  #forceBreak .= True
  pure p

-- basically if we see something which would cause us to break the word *now*
-- it's all on a lookahead anyway so it's not a problem if we are greedy in what we look for
-- but this is the inverse of the inform parser; they check if they do *not* break whereas we
-- check for a breakage...this is therefore inverted
standalonePunctuationOrSpace ::
  Parser m
  => m ()
standalonePunctuationOrSpace = choice
    [ -- whitespace
      void $ satisfy (\x -> isSpace x || x == '[') >> takeWhileP Nothing (\x -> isSpace x || x == '[' || x == '"')
      -- some literal mode entering punctuation
    , void $ string' "(-"
    , isPunctuationNoSlash
    ]

-- note that because / isn't punctuation, then reading :/ as in http:// means that only the colon
-- needs to be checked to ensure no, we do not wish to break. once we get to the //, it has no problems
-- just accepting it
isPunctuationNoSlash ::
  Parser m
  => m ()
isPunctuationNoSlash = do
  void $ satisfy (`member` standardPunctuation)
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
  => m Text
surroundedPunctuation = try $ do
  a <- satisfy (\x -> isLower x || isDigit x) -- read either a digit or a lowercase letter
  b <- satisfy (`member` standardPunctuation) -- read the internal bit of the punctuation
  -- once again read a lowercase or digit
  c <- satisfy (\x -> (isLower x && b == '.' && isLower a) || (isDigit a && isDigit x))
  pure $ toText [a,b,c]

betterSurround ::
  Parser m
  => Int
  -> Text
  -> Text
  -> m a
  -> m a
betterSurround o e betterErrorMsg inner  = do
  let s = toString ("We found the start of a " <> betterErrorMsg <> " here which was never closed. \
  \Perhaps you forgot a `" <> e <> "`?")
  region (\case
    (TrivialError _ (Just EndOfInput) _) ->
      FancyError o (Set.singleton $ ErrorFail s)
    x -> x ) inner

comment ::
  Parser m
  => m ()
comment = do
  o <- getOffset
  single '['
  betterSurround o "]" "comment" $ do
    manyTill (void comment <|> void anySingle) (single ']')
    pass

annotateToken ::
  (MonadParsec e Text m, MonadState LexerState m)
  => m a
  -> m (SourceLocation, a)
annotateToken p = do
  fn <- gets currentFilename
  b <- getSourcePos
  b' <- getOffset
  r <- p
  a <- getSourcePos
  a' <- getOffset
  return (SourceLocation fn (Just ((b', b), (a', a))) 0, r)