module Uninformed.Parser.Driver
  ( runParser
  , Parser(..)
  ) where

import Uninformed.Parser.Parser
import Uninformed.Prelude hiding (many)
import Text.Megaparsec hiding (runParser)
import Uninformed.Parser.Extensions
import Uninformed.Parser.Headings
import Uninformed.Parser.NewVerb
import Uninformed.Parser.Types
import qualified Data.Map.Strict as Map

data ParseNode = ParseNode

parseFile
  :: Parser AST
parseFile = error ""

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
{-}
parseExtension :: Parser Extension
parseExtension = do
  h <- parseExtensionHeader
  paragraphBreak
  r <- optional rubric
  -- and some optional stuff
  parseSourceBody
  fail ""
-}
parseSourceBody :: Parser [ParseNode]
parseSourceBody = many $ do
  ParseNode <$ parseHeading
  <|> (ParseNode <$ parseNewVerb)
