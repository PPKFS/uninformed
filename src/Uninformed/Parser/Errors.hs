module Uninformed.Parser.Errors
  ( startSnippet
  , endSnippetAtParagraphBreak
  , errorSnippet
  , buildSnippet
  , makeDiagnostic

  , unexpectedNewlineError
  , unexpectedNewlineMsg
  , unexpectedPhraseTokenError
  , expectedEndOfSentence
  , unexpectedSentence
  , unexpectedPunctuationInHeading
  , unexpectedPunctuationInHeadingMsg
  , amendLiteralModeMsg
  , phraseUnexpectedTokenMsg) where
  
import Uninformed.Parser.Types
import Uninformed.Prelude
import Optics
import Chapelure.Types
import qualified Data.Vector.NonEmpty as NEVec
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Text.Megaparsec
import Data.Text.Display
import Optics.State.Operators
import Uninformed.Parser.Combinators

-- | Mark the current input position as the start of a code snippet.
startSnippet :: 
  Parser () -- ^ how to end this snippet
  -> Parser ()
startSnippet sEnd = do
  --update the posState machinery
  void getSourcePos
  ps <- statePosState <$> getParserState
  snippetHandler % snippetStart .= ps
  snippetHandler % snippetEnding .= sEnd

endSnippetAtParagraphBreak :: Parser ()
endSnippetAtParagraphBreak = lookAhead paragraphBreak

errorSnippet :: 
  Parser a -- ^ the error that could be satisfied
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
      _ <- anySingle `manyTill` (eof <|> eos)
      i3 <- getOffset
      let len = i3-snipBeg
          snip = fst <$> takeN_ len (pstateInput $ _snippetStart sh)
          fp = _snippetFilename sh
          snippet = buildSnippet
            fp
            (fromMaybe "Somehow ran out of input.." snip)
            (unPos $ sourceLine s)
            (unPos $ sourceColumn s)
            [(unPos $ sourceLine s, i-snipBeg, i2-snipBeg, errMsg x)]
      parseError (FancyError i $ one $ ErrorCustom $ UninformedParseError snippet curCxt)

buildSnippetContext :: 
  ParseErrorType
  -> Parser Text
buildSnippetContext eType = do
  let dis = display eType
  cxt <- use $ snippetHandler % snippetContext
  let builtCxt = intercalate ", " cxt
  return $ builtCxt <> ", " <> dis <> "."
  
buildSnippet :: 
  Text -- ^ the filename
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
    , content = Vec.fromList (lines (T.strip snip))
    }

makeDiagnostic :: 
  Text
  -> Snippet
  -> Diagnostic
makeDiagnostic hlp snip = Diagnostic Nothing Error (Just hlp) Nothing
  (Just $ NEVec.singleton snip)

unexpectedNewlineError :: Parser ()
unexpectedNewlineError = do
  ilm <- use inLiteralMode
  errorSnippet
    (do
      void $ takeWhile1P Nothing (`elem` newlineCharacters)
      use inLiteralMode)
    (if ilm then MultipleParseErrors [UnexpectedToken, MissingQuoteEnd] else UnexpectedToken)
    (\b -> unexpectedNewlineMsg <> (if b then amendLiteralModeMsg else ""))

amendLiteralModeMsg :: Text
amendLiteralModeMsg = " We were in the middle of reading a quoted string; perhaps you forgot a \"?"

unexpectedNewlineMsg :: Text
unexpectedNewlineMsg = "When reading something, we found a newline in a phrase where newlines aren't allowed."

unexpectedPhraseTokenError :: Parser a
unexpectedPhraseTokenError = do
  errorSnippet
    anySingle
    UnexpectedToken
    (const phraseUnexpectedTokenMsg)
  fail "shouldnt get this far"

phraseUnexpectedTokenMsg :: Text
phraseUnexpectedTokenMsg = "When reading a phrase, found a character I wasn't expecting."
-- | A phrase consists of at least one word, with some possible errors, separated by whitespace.

expectedEndOfSentence :: Parser ()
expectedEndOfSentence = errorSnippet
  (do
      startSnippet endSentence
      someTill_ anySingle (lookAhead $ satisfy $ \x -> x == '.')
      )
    UnexpectedToken
    (const "We expected the sentence to end, but we found this part of a sentence.")


unexpectedSentence :: Parser ()
unexpectedSentence = do
  errorSnippet
    (do
      startSnippet endSentence
      someTill_ anySingle (lookAhead $ satisfy $ \x -> x == '.')
      )
    UnexpectedToken
    (const "When running a parser, we encountered this sentence and had no idea how to parse it.")

unexpectedPunctuationInHeading :: Parser ()
unexpectedPunctuationInHeading = errorSnippet
  (do
    guardM (not <$> use inLiteralMode) --if we're in literal mode, we'll allow it.
    takeWhile1P Nothing (`elem` sentenceEndingPunctuation))
  UnexpectedToken
  (const unexpectedPunctuationInHeadingMsg)

unexpectedPunctuationInHeadingMsg :: Text
unexpectedPunctuationInHeadingMsg = "Some sentence-ending punctuation was found (;, :, or .). To use punctuation in a heading, wrap it in double-quotes."
