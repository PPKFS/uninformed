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
import Data.Text.Display

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
parseNewVerb = verbPhrase
  (const "A new verb declaration")
  (specifically' "The verb to")
  ["implies", "means"]
  conjugationsAndRelation
  (const $ NewVerbDeclaration " ")


conjugationsAndRelation :: Parser Text
conjugationsAndRelation = do
  vk <- parseVerbKind
  --p <- highlightStart
  (vn, (mConjug, impOrMens)) <- first (display vk <>) <$> phrase [] (do
    conj <- optional parseVerbConjugations
    r <- True <$ specifically' "implies" <|> False <$ specifically' "means"
    return (conj, r))
  rel <- relationPart
  return ""

relationPart :: Parser a0
relationPart = error "not implemented"

parseVerbConjugations :: Parser a1
parseVerbConjugations = error "not implemented"

parseVerbName :: Parser a2
parseVerbName = error "not implemented"

data VerbKind = AbleTo | Prepositional | Regular

instance Display VerbKind where
  displayBuilder AbleTo = "to be able to"
  displayBuilder Prepositional = "to be"
  displayBuilder Regular = "to"
parseVerbKind :: Parser VerbKind
parseVerbKind = AbleTo <$ try (specifically' "be able to")
  <|> Prepositional <$ try (specifically' "be")
  <|> Regular <$ pass


{-

-- this is equivalent to "before" in the 2 parse nodes of parse_new_verb
-- TODO: check for overlap, to explicitly call out 'to be'?
verbName :: Parser (Text, (VerbConjugationTable, Bool))
verbName = do
  parseWord'_ "to" <?> "verb infinitive"
  vp <- (try (parseWords'_ "be able to") >> regularVerb True) <|>
    (try (parseWord'_ "be") >> prepositionalVerb) <|>
    regularVerb False
  c <- buildConjugationTable . fromMaybe [] <$> optional verbConjugation
  
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


-}
{-
findInSentence'
  :: Text
  -> Parser (Text, Text)
findInSentence' breakWord = try $ do
  w <- manyTill
-}
