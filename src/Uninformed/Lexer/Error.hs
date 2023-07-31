{-# OPTIONS_GHC -Wno-orphans #-}

module Uninformed.Lexer.Error
  ( makeError
  , StructuredError
  ) where

import Uninformed.Prelude
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle)

type StructuredError = Diagnostic Text

instance HasHints Void msg where
  hints _ = mempty

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