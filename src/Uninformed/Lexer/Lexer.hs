{-# LANGUAGE RecordWildCards #-}

module Uninformed.Lexer.Lexer
  ( LexerInput(..)
  , lex
  ) where

import Uninformed.Prelude hiding ( gets, get, modify, state )

import Control.Monad.State ( runState, state)
import Data.Char ( isDigit, isPunctuation, isLower )
import Data.Set ( member )

import Optics.State ( use )
import Optics.State.Operators ((.=))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char ( string' )

import Uninformed.Word
import Uninformed.SourceFile
import Uninformed.Vocabulary

import qualified Data.HashMap.Strict as HM
import Error.Diagnose.Compat.Megaparsec
import Error.Diagnose
import Uninformed.Lexer.Combinators

type LexerPipelineStage i o = i -> Either StructuredError o

defaultLexerState :: LexerState
defaultLexerState = LexerState
  { previousWhitespace = Newline
  , forceBreak = False
  , currentFilename = Nothing
  }

data LexerInput = LexerInput
  { divideLiteralsAtSubstitutions :: Bool
  , sourceFilename :: Maybe Text
  , textStream :: Text
  }

type StructuredError = Diagnostic Text

makeError ::
  Text
  -> Text
  -> ParseErrorBundle Text Void
  -> StructuredError
makeError filename content bundle = let
  -- Creates a new diagnostic with no default hints from the bundle returned by megaparsec
  diag = errorDiagnosticFromBundle Nothing "Error during lexical analysis" Nothing bundle
   -- Add the file used when parsing with the same filename given to 'MP.runParser'
  in
    addFile diag (toString filename) (toString content)


lex :: LexerPipelineStage LexerInput (SourceFile [Token], VocabMap)
lex LexerInput{..} = do
  let sfn = fromMaybe "<interactive>" sourceFilename
      -- mostly for the quirks of inform, we want to amend some whitespace to better mimic it for test reasons.
      updatedInput = "\n" <> textStream  <> " \n\n\n\n "
      lexer :: StateT LexerState (Parsec Void Text) [Token]
      lexer = do
        -- this is more of an inform quirk than anything..because it starts the lexer with a default of
        -- newline as the last seen space, then reading a single newline at the start of a file makes a paragraph break.
        -- ...yeah...
        pb <- initialNewlines
        (pb ++) . mconcat <$> manyTill (do
          -- lex the actual word
          w <- parseToken divideLiteralsAtSubstitutions
          -- the only cases where whitespace is required (breaking an ordinary word) is dealt with, but we may have additional whitespace.
          -- we don't want to double check the whitespace after we've broken for a punctuation mark because we don't want to add an excess space
          -- this mimics inform's "add in extra spaces everywhere".
          (lookAhead (satisfy isPunctuation) >> pass) <|> whenJustM (optional admireWhitespace) (#previousWhitespace .=)
          pure w) eof
  tokenStream <- first (makeError sfn updatedInput) $ -- convert the error from a megaparsec one to a
                  parse (evalStateT lexer defaultLexerState) (toString sfn) updatedInput
  let (amendedTokens, vocabularyTable) = first
        -- add the newly identified vocabulary hashes to the tokens
        (zipWith (\t i -> t & #vocabularyEntry .~ i) tokenStream . map hashCode)
        -- identify each word and amend it to the map
        $ runState (mapM (state . identify . word) tokenStream) HM.empty
  pure (SourceFile
      { filename = sourceFilename
      , rawText = textStream
      , contents = amendedTokens
      , fileQuotedWordCount = sum $ map (quotedWordCount . word) amendedTokens
      , fileRawWordCount = length amendedTokens
      , fileWordCount = sum $ map (wordCount . word) amendedTokens
      }, vocabularyTable)

parseToken ::
  Parser m
  => Bool
  -> m [Token]
parseToken spl = choice
  [ [] <$ comment
  , makeParagraphBreaks <$> pbreak
  , stringLit spl
  , one <$> i6Inclusion
  , one <$> ordinaryWord
  ]

ordinaryWord ::
  Parser m
  => m Token
ordinaryWord = do
  pw <- use #previousWhitespace
  (srcL, w) <- annotateToken $ do
    w <- mconcat <$> someTill (choice [
      surroundedPunctuation
      , one <$> punctuationThenLetter
      , one <$> anySingle
      ]) (guardM (use #forceBreak) <|> lookAhead standalonePunctuationOrSpace)
    ifM (use #forceBreak)
      (#forceBreak .= False >> #previousWhitespace .= Space)
      (admireWhitespace >>= (#previousWhitespace .=))
    return $ OrdinaryWord w
  pure (Token srcL w pw (-1))

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

i6Inclusion ::
  Parser m
  => m Token
i6Inclusion = do
  (srcL, w) <- annotateToken $ do
    o <- getOffset
    string' "(-"
    betterSurround o "-)" "Inform6 inclusion" $ do
      I6 . toText <$> manyTill anySingle (string' "-)")
  #previousWhitespace .= Space
  pure (Token srcL w Space (-1))

stringLiteralInternal ::
  Parser m
  => m (SourceLocation, Word)
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
  -> m [Token]
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
    (s, []) -> pure [Token s (StringLit "") pw (-1)]
    (_, (sx, wx):xs) -> pure $ Token sx wx pw (-1) : map (\(sx', wx') -> Token sx' wx' Space (-1)) xs
