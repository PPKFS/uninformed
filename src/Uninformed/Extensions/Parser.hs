module Uninformed.Extensions.Parser where

import Uninformed.Prelude hiding (some)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char ()
import Uninformed.Extensions.Types
import Uninformed.Parser.Types
import Uninformed.Parser.Parser
import Text.Megaparsec
import Uninformed.Parser.Combinators
import Uninformed.Parser.Expressions

parseExtensionHeader :: Parser ExprLoc
parseExtensionHeader = annotateLocation $ withoutNewlines $ do
  -- optionally, we can try to interpret a version header.
  vn <- try $ optional (do
    specifically' "version"
    vn <- parseVersionNumber
    specifically' "of"
    return vn)
  t' <- fst <$> headerLikePhrase (specifically "by")
  a <- fst <$> headerLikePhrase (specifically "begins here")
  endSentence
  return $ ExtensionHeaderExpr (ExtensionHeader (ExtensionName (toText t')  (toText a)) vn)

parseVersionNumber :: Parser ExtensionVersion
parseVersionNumber = do
  i <- decimal
  sn <- optional (do
    void $ single '/'
    (decimal :: Parser Integer))
  void hspace1
  return $  case sn of
    Nothing -> if i > 999 then ExtensionVersion 1 (Just i) else ExtensionVersion i Nothing
    Just ser -> ExtensionVersion i (Just $ fromIntegral ser)

parseStandaloneQuotedLine :: Parser Text
parseStandaloneQuotedLine = withNewlines $ do
  s <- rawStringLiteral False
  paragraphBreak
  return s
