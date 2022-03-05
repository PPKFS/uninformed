module Uninformed.Parser.Driver
  ( runParser
  , parseExtension
  , Parser(..)
  ) where

import Uninformed.Prelude hiding (some, many)
import Text.Megaparsec hiding (runParser)
import Uninformed.Headings.Parser
import Uninformed.Parser.Types
import qualified Data.Map.Strict as Map
import Uninformed.Extensions.Types
import Uninformed.Extensions.Parser
import Uninformed.Parser.Combinators
import Uninformed.Parser.Errors
import Uninformed.Parser.Parser (annotateLocation)

data ParseNode = ParseNode

runParser
  :: (ParseState -> ParseState)
  -> Parser a
  -> Text
  -> IO (Either (ParseErrorBundle Text UninformedParseError) a)
runParser f (Parser p) = runParserT (evalStateT p (f initParser)) ""

initParser :: ParseState
initParser = ParseState False False
  (SnippetHandler
    (PosState "No input has been given." 0 (initialPos "untitled.inform") pos1 "")
      (void $ takeWhile1P Nothing (`elem` sentenceEndingPunctuation )) "Untitled.hs" [])
  Map.empty ()

parseExtension :: Parser ExprLoc
parseExtension = annotateLocation $ do
  h <- parseExtensionHeader
  paragraphBreak
  rubric <- optional parseStandaloneQuotedLine
  parseSourceBody
  return $ ExtensionExpr Extension
  
parseSourceBody :: Parser [ParseNode]
parseSourceBody = some $ do
  ParseNode <$ parseHeading
  -- <|> (ParseNode <$ parseNewVerb)
  <|> ParseNode <$ unexpectedSentence
