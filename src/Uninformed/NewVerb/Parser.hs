module Uninformed.NewVerb.Parser
( parseNewVerb
) where

-- "The verb to eat..." - 4/verb.11

import Uninformed.Parser.Parser
import Uninformed.Prelude
import Text.Megaparsec
import Uninformed.Parser.Types
import Uninformed.NewVerb.Types
import Uninformed.Parser.Expressions
import Uninformed.Parser.Errors
import Optics
import Uninformed.Parser.Combinators

parseNewVerb :: Parser ExprLoc
parseNewVerb = do
  endSnippetAtParagraphBreak 
  annotateLocation $ verbPhrase
    (const "A new verb declaration")
    (specifically' "The verb to")
    ["implies", "means"]
    parseVerbDeclaration
    (\() e -> NewVerbDeclarationExpr e)

parseVerbDeclaration :: Parser (NewVerbDeclaration ExprLoc)
parseVerbDeclaration = do
  nvd <- annotateLocation $ NewVerbDefinitionExpr <$> parseNewVerbDefinition
  r <- Implies <$ specifically' "implies" <|> Means <$ specifically' "means"
  rel <- annotateLocation $ BinaryPredicateExpr <$> parseRelationPart
  let decl = NewVerbDeclaration nvd r rel
  -- success!
  registerNewVerb decl
  return decl

registerNewVerb :: NewVerbDeclaration ExprLoc -> Parser ()
registerNewVerb = error "not implemented"

parseNewVerbDefinition :: Parser NewVerbDefinition
parseNewVerbDefinition = do
  vk <- parseVerbKind
  vn <- parseVerbName
  conj <- optional parseVerbConjugations
  let nvd = NewVerbDefinition vk vn conj
  validateVerb nvd --we don't register it just yet, just check it doesn't exist and is therefore a dupe
  return nvd

parseVerbKind :: Parser VerbKind
parseVerbKind = AbleTo <$ try (specifically' "be able to")
  <|> Prepositional <$ try (specifically' "be")
  <|> Regular <$ pass

parseVerbName :: Parser Text
parseVerbName = do
  markSnippetStart
  fst <$> phrase []
    (lookAhead $
      specificallySymbol' "("
      <|> specifically' "implies"
      <|> specifically' "means")

-- check it's not present in the table
validateVerb :: NewVerbDefinition -> Parser ()
validateVerb vName = guardM $ isJust <$> use (verbUsages % at (verbDefinitionFullName vName))

parseRelationPart :: Parser BinaryPredicate
parseRelationPart = do
  markSnippetStart
  specifically' "the"
  isRev <- isJust <$> optional (specifically' "reversed")
  r <- phrase [] (specifically "relation" <|> specifically "property")
  endSentence
  bp <- uncurry (getBinaryPredicate isRev) r
  --verify it is indeed a relation or a property we know of
  return $ BinaryPredicate

getBinaryPredicate :: Bool -> Text -> Text -> Parser a0
getBinaryPredicate = error "not implemented"
parseVerbConjugations :: Parser a1
parseVerbConjugations = error "not implemented"

registerVerbUsage
  :: Text
  -> Bool
  -> Tense
  -> BinaryPredicate
  -> Parser ()
registerVerbUsage name negat tens bp = do
  pass



{-
prepositionalVerb :: Parser a0
prepositionalVerb = error "not implemented"

-- 〈Reject with a problem message if verb has no auxiliary and is not conjugated 28〉
-- but...why
regularVerb :: Bool -> Parser a0
regularVerb isToBeAbleTo = do
  vn <- parsePhraseTillAhead (void (single '(') <|> void (string' "implies") <|> void (string' "means"))
  let fullVerbName = (if isToBeAbleTo then ("be able to " <>) else id) vn
  alreadyDefined <- findVerbUsage fullVerbName
  fail ""

  --so we flip on whether or not it's "to be able to X" to check for verb usage
  -- if it's duplicated, then we track where the original one was, OR built-in




-}
{-
findInSentence'
  :: Text
  -> Parser (Text, Text)
findInSentence' breakWord = try $ do
  w <- manyTill
-}
