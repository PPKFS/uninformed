module Uninformed.Parser.NewVerb
( NewVerbDeclaration(..)
, parseNewVerb

) where

-- "The verb to eat..." - 4/verb.11

import Uninformed.Parser.Parser
import Uninformed.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char (string')
import qualified Data.Map.Strict as Map
import Uninformed.Parser.Types
import Optics

data NewVerbDeclaration = NewVerbDeclaration Text Text deriving stock (Eq, Show)

data BinaryPredicate = BinaryPredicate
registerVerbUsage
  :: Text
  -> Bool
  -> Tense
  -> BinaryPredicate
  -> Parser ()
registerVerbUsage name negat tens bp = do
  pass


parseNewVerb :: Parser NewVerbDeclaration
parseNewVerb = error ""

{-do
  try $ parseWords'_ "the verb"
  (verbName, (conjugations, impOrMeans)) <- verbName
  return $ NewVerbDeclaration verbName (show impOrMeans)

-- this is equivalent to "before" in the 2 parse nodes of parse_new_verb
-- TODO: check for overlap, to explicitly call out 'to be'?
verbName :: Parser (Text, (VerbConjugationTable, Bool))
verbName = do
  parseWord'_ "to" <?> "verb infinitive"
  vp <- (try (parseWords'_ "be able to") >> regularVerb True) <|>
    (try (parseWord'_ "be") >> prepositionalVerb) <|>
    regularVerb False
  c <- buildConjugationTable . fromMaybe [] <$> optional verbConjugation
  r <- True <$ string' "implies" <|> False <$ string' "means"
  return (vp, (c, r))

buildConjugationTable :: [VerbConjugation] -> VerbConjugationTable
buildConjugationTable = error ""

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

-- do we have a verb usage already defined for this phrase?
findVerbUsage :: Text -> Parser (Maybe (Set VerbUsage))
findVerbUsage vName = use (verbUsages % at vName)

-- 5/conj 30
verbConjugation :: Parser [VerbConjugation]
verbConjugation = inParentheses $ sepBy (do
  pronoun <- Singular <$ parseWordOneOf' ["he", "she", "it"] <|>
             They <$ parseWord'_ "they"
  isParticiple <- isJust <$> optional (inParentheses (parseWord'_ "is"))
  conjug <- parsePhraseTillAhead (single ',' <|> single ')')
  let participle = if
    isParticiple then
        (if
          any (isSuffixOf' "ing" ) (words conjug)
        then Just Present
        else Just Past)
      else Nothing

  adjectival <- isJust <$> optional (inParentheses (parseWord'_ "adjectival"))
  return $ VerbConjugation pronoun participle conjug adjectival) (parseWord'_ ",")



{-
findInSentence'
  :: Text
  -> Parser (Text, Text)
findInSentence' breakWord = try $ do
  w <- manyTill
-}
-}