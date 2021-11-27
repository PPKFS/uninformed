module Uninformed.Parser.Driver
  ( runParser
  , Parser(..)
  ) where

import Uninformed.Parser.Common
import Uninformed.Prelude
import Text.Megaparsec (ParseErrorBundle, runParserT)

parseFile 
  :: Parser AST
parseFile = error ""

runParser
  :: Parser a
  -> Text
  -> IO (Either (ParseErrorBundle Text UninformedParseError) a)
runParser (Parser p) = runParserT (evalStateT p ParseState) ""