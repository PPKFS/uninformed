{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Uninformed.Words.Lexer
  ( SourceLocation(..)
  , Whitespace(..)
  , lex
  , displayWord
  , blankWord
  ) where

import Uninformed.Prelude hiding ( gets, get, modify, state )
import Data.Char ( isSpace, isDigit, isPunctuation, isLower )
import Data.Set (member)

import Text.Megaparsec
import Text.Megaparsec.Char ( string' )
import Uninformed.Words.Vocabulary ( identify, VocabType(..), VocabMap, PunctuationSet (..), getPunctuation )
import qualified Data.HashMap.Strict as HM
import Uninformed.Words.Lexer.Types
import Uninformed.Words.TextFromFiles ( SourceFile(..), wordCount, quotedWordCount )
import Control.Monad.State ( runState, state, gets )
import Optics.State.Operators ((.=))
import Optics.State (use)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import qualified Data.Set as Set

data LexerState = LexerState
  { previousWhitespace :: Whitespace
  , forceBreak :: Bool
  , currentFilename :: Maybe Text
  } deriving stock (Generic)

defaultLexerState :: LexerState
defaultLexerState = LexerState
  { previousWhitespace = Newline
  , forceBreak = False
  , currentFilename = Nothing
  }

type Parser m = (MonadState LexerState m, MonadParsec Void Text m)

--- * Whitespace handling

isNewline :: Char -> Bool
isNewline x = x `elem` ['\n', '\DEL', '\r']

isHspace :: Char -> Bool
isHspace x = isSpace x && not (isNewline x)

-- | match anything that isn't whitespace
nonWhitespace ::
 Parser m
  => m Char
nonWhitespace = satisfy (not . isSpace)

space ::
  Parser m
  => m Char
space = satisfy isSpace

type StructuredError = Diagnostic Text
type PipelineStage i o = i -> Either StructuredError o

lex :: PipelineStage LexerInput (SourceFile [Word], VocabMap)
lex LexerInput{..} =
  bimap (makeError sfn updatedInput) listToSourceFile $
    parse (evalStateT lexer defaultLexerState) (toString sfn) updatedInput
  where
    sfn = fromMaybe "<interactive>" sourceFilename
    updatedInput = "\n" <> textStream  <> " \n\n\n\n "
    ps = StandardPunctuation
    lexer :: StateT LexerState (Parsec Void Text) [Word]
    lexer = do
      -- this is more of an inform quirk than anything..because it starts the lexer with a default of
      -- newline as the last seen space, then reading a single newline at the start of a file makes a paragraph break.
      -- ...yeah...
      pb <- initialNewlines ps
      (pb ++) . mconcat <$> manyTill (do
        -- lex the actual word
        w <- parseWord divideLiteralsAtSubstitutions ps
        -- the only cases where whitespace is required (breaking an ordinary word) is dealt with, but we may have additional whitespace.
        -- we don't want to double check the whitespace after we've broken for a punctuation mark because we don't want to add an excess space
        -- this mimics inform's "add in extra spaces everywhere".
        (lookAhead (satisfy isPunctuation) >> pass) <|> whenJustM (optional (admireWhitespace ps)) (#previousWhitespace .=)
        pure w) eof
    mkVocabMap wordList = first (zip wordList) $ runState (mapM (state . identify . word) wordList) HM.empty
    listToSourceFile wordList = let (_, vm) = mkVocabMap wordList in (SourceFile
      { sourceFileName = sourceFilename
      , sourceFileText = textStream
      , sourceFileData = wordList
      , sourceFileQuotedWordCount = sum $ map (quotedWordCount . word) wordList
      , sourceFileRawWordCount = length wordList
      , sourceFileWordCount = sum $ map (wordCount . word) wordList
      }, vm)

instance HasHints Void msg where
  hints _ = mempty
makeError ::
  Text
  -> Text
  -> ParseErrorBundle Text Void
  -> StructuredError
makeError filename content bundle =
  let diag  = errorDiagnosticFromBundle Nothing "Error during lexical analysis" Nothing bundle
           --   Creates a new diagnostic with no default hints from the bundle returned by megaparsec
      diag' = addFile diag (toString filename) (toString content)
                 --   Add the file used when parsing with the same filename given to 'MP.runParser'
  in diag'

initialNewlines ::
  Parser m
  => PunctuationSet
  -> m [Word]
initialNewlines ps = do
  mbPbreaks <- makeParagraphBreaks <$$> ((Nothing <$ optional (admireWhitespace ps)) <|> optional (try pbreak))
  pure $ fromMaybe [] mbPbreaks

makeParagraphBreaks ::
  (Whitespace, Int, SourceLocation)
  -> [Word]
makeParagraphBreaks (w, n, srcLoc) = replicate n (Word srcLoc ParagraphBreak w)

parseWord ::
  Parser m
  => Bool
  -> PunctuationSet
  -> m [Word]
parseWord spl ps = choice
  [ [] <$ comment
  , makeParagraphBreaks <$> pbreak
  , stringLit spl
  , one <$> i6Inclusion
  , one <$> ordinaryWord ps
  ]

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
      ws <- reverse <$> someTill space (try $ lookAhead (void nonWhitespace <|> void pbreak <|> eof))
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

longestSpan :: (a -> Bool) -> [a] -> Int
longestSpan f lst = fst $ foldl'
  (\(best, cur) v -> if f v then (if cur >= best then cur+1 else best, cur+1) else (best, 0)) (0, 0) lst

findMostSignificantWhitespace :: (String, String) -> Whitespace
findMostSignificantWhitespace (run, mbNewline) = case longestSpan (=='\t') run of
  0 -> if null mbNewline then Space else Newline
  x -> if null mbNewline then Tab else TabIndent x

ordinaryWord ::
  Parser m
  => PunctuationSet
  -> m Word
ordinaryWord ps = do
  pw <- use #previousWhitespace
  (srcL, w) <- annotateToken $ do
    w <- mconcat <$> someTill (choice [
      surroundedPunctuation ps
      , one <$> punctuationThenLetter ps
      , one <$> anySingle
      ]) (guardM (use #forceBreak) <|> lookAhead (standalonePunctuationOrSpace ps))
    ifM (use #forceBreak)
      (#forceBreak .= False >> #previousWhitespace .= Space)
      (admireWhitespace ps >>= (#previousWhitespace .=))
    return $ OrdinaryWord w
  pure (Word srcL w pw)

punctuationThenLetter ::
  Parser m
  => PunctuationSet
  -> m Char
punctuationThenLetter ps = try $ do
  p <- satisfy (`member` getPunctuation ps)
  lookAhead (satisfy (\x -> not (x `member` getPunctuation ps)))
  #forceBreak .= True
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
      void $ satisfy (\x -> isSpace x || x == '[') >> takeWhileP Nothing (\x -> isSpace x || x == '[' || x == '"')
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
  -- once again read a lowercase or digit
  c <- satisfy (\x -> (isLower x && b == '.' && isLower a) || (isDigit a && isDigit x))
  pure $ toText [a,b,c]

i6Inclusion ::
  Parser m
  => m Word
i6Inclusion = do
  (srcL, w) <- annotateToken $ do
    o <- getOffset
    string' "(-"
    betterSurround o "-)" "Inform6 inclusion" $ do
      I6 . toText <$> manyTill anySingle (string' "-)")
  #previousWhitespace .= Space
  pure (Word srcL w Space)

stringLiteralInternal ::
  Parser m
  => m (SourceLocation, VocabType)
stringLiteralInternal = do
  annotateToken $ do
    t <- toText <$> manyTill (
      '\n' <$ try (takeWhileP Nothing isHspace >> satisfy isNewline >> takeWhileP Nothing isHspace) <|>
      ' ' <$ single '\160' <|>
      anySingle) (lookAhead $ single '"')
    pure $ StringLit t

stringLit ::
  Parser m
  => Bool
  -> m [Word]
stringLit spl = do
  pw <- use #previousWhitespace
  ls <- annotateToken $ do
    o <- getOffset
    single '"'
    betterSurround o "\"" "double quoted string" $ do
      manyTill (
        if spl
          then annotateToken (StringSub <$> (single '[' >> toText <$> manyTill anySingle (single ']')))
          <|> stringLiteralInternal
          else stringLiteralInternal) (single '"')
  #previousWhitespace .= Space
  case ls of
    (s, []) -> pure [Word s (StringLit "") pw]
    (_, (sx, wx):xs) -> pure $ Word sx wx pw : map (\(sx', wx') -> Word sx' wx' Space) xs

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