module Uninformed.Parser.Extensions
  ( {-ExtensionName(..)
  , ExtensionHeader(..)
  , ExtensionVersion(..)
  , Extension(..)
  , rubric
  , parseExtensionHeader-}
  ) where

import Uninformed.Prelude hiding (some)
import Uninformed.Parser.Parser
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (hspace1)

{-}


data ExtensionVersion = ExtensionVersion
  { _extensionRelease :: Int
  , _extensionSerial :: Maybe Int
  } deriving stock (Eq, Show)

data ExtensionHeader = ExtensionHeader
  { _extensionName :: ExtensionName
  , _extensionVersion :: Maybe ExtensionVersion
  } deriving stock (Eq, Show)

data Extension = Extension deriving stock (Eq, Show)

parseExtensionHeader :: Parser ExtensionHeader
parseExtensionHeader = do
  -- optionally, we can try to interpret a version header.
  vn <- try $ optional (do
    parseWord'_ "version"
    vn <- parseVersionNumber
    parseWord'_ "of"
    return vn)
  t' <- parsePhraseTillWord "by"
  a <- parsePhraseTillWords "begins here"
  endSentence
  return (ExtensionHeader (ExtensionName (toText t')  (toText a)) vn)

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

rubric :: Parser Text
rubric = do
  s <- quotedString
  paragraphBreak
  return s
  -}