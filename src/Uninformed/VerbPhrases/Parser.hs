module Uninformed.VerbPhrases.Parser where

import Uninformed.Parser.Expressions
import Uninformed.Parser.Types
import Uninformed.Parser.Errors
import Uninformed.Parser.Combinators
import Text.Megaparsec hiding (some, many)
import Uninformed.Prelude
import Optics
import Uninformed.Parser.Parser
import Data.List (zipWith3)

parseVerbPhrase :: Parser ExprLoc
parseVerbPhrase = annotateLocation $ do
  startSnippet endSentence
  possibleVerbs <- tryVerbs
  case possibleVerbs of
    [] -> unexpectedPhraseTokenError
    --if we have only one possible phrase, we just go with that
    [(v, p, f)] -> f
    --otherwise, we note there's a degree of ambiguity and try each in turn
    x -> choice (map (view _3) x)
{-
  -- and then we check each of the kinds of verb phrase we want.
  --ignored: unicode translation.
  understandOrTest
  <|> debuggingLog
  <|> sceneAnchoring
  <|> newNotation
  <|> resourceDeclaring
  <|> actionOrActivity
  <|> verbOrRelationDeclaration
  <|> propertyDeclaration
  <|> rulePositioning
  <|> assertionAboutThings
  <|> haskellTranslation
  <|> useOrRelease
  <|> pastTenseError
-}

data PhraseAccumulator = PhraseAccumulator
  { _phraseAccCurrent :: [(Phrase, NonEmpty Text)] -- a list of the total phrase and what we have left to find
  , _phraseAccFound :: [Phrase] -- we succeeded in these
  , _phraseAccAll :: [Phrase]
  }

data Phrase = Phrase
  { _phraseRank :: Int
  , _phraseBody :: NonEmpty Text
  , _phraseBeginsWith :: [Text]
  , _phraseParser :: Parser ExprLoc
  }

-- we want to go through every word, seeing if we can make a guess about it being a verb phrase we're after
-- for each word, we can either: find an entire phrase, or find the next part of a phrase; if we finished finding the
-- phrase, we note that it succeeded
tryVerbs :: Parser [Phrase]
tryVerbs = lookAhead $ do
  wordList <- sentenceConsistingOf (some $ word False)
  initAcc <- makePossibleVerbs
  --let's only include possible verbs which can be anywhere, or we have a matching prefix
  let filtAcc = filter ((`isPrefixOf` wordList) . _phraseBeginsWith) ( _phraseAccAll initAcc)
  pure . sortOn Down . _phraseAccFound $ foldl' updateAcc (initAcc { _phraseAccAll = filtAcc}) wordList

makePossibleVerbs :: Parser PhraseAccumulator
makePossibleVerbs = do
  --get all verbs defined thus far TODO
  --which has some kind of tense-stuff
  --plus the inbuilts
  return $ PhraseAccumulator [] [] $
    zipWith (\x (y, z, f) -> Phrase x y z f) [1..] [
      ("translates" :| ["into", "unicode"], [], translatesIntoUnicode)
    , ("as" :| [], ["Understand"], understandAs)
    , ("with" :| [], ["Test"], testWith)
    , ("in" :| ["the", "debugging"], ["Include"], debugging)
    , ("from" :| ["the", "debugging"], ["Omit"], debugging)
    , ("at" :| [], ["Document"], documentAt)
    , ("begins" :| ["when"], [], sceneAnchoring)
    , ("ends" :| ["when"], [], sceneAnchoring)
    , ("when" :| [], [], sceneAnchoring) --iffy because we need an "ends..." in here too but with possible other stuff
    , ("specifies" :| [], [], specifies)
    , ("are" :| ["defined", "by"], [], definedBy)
    , ("story" :| ["is", "episode"], ["The", "story", "is", "episode"], episode)
    , ("is" :| [], ["The", "plural", "of"], thePluralOf)
    , ("is" :| ["the", "file"], ["Figure"], fileHandling)
    , ("is" :| ["the", "file"], ["Sound", "effect"], fileHandling)
    , ("is" :| ["the", "file"], ["Sound"], fileHandling)
    --missing the "text/binary" stuff here too
    , ("is" :| ["an", "activity"], [], newActivity)
    , ("is" :| ["an", "action"], [], newAction)
    , ("implies" :| [], ["The", "verb", "to"], newVerb)
    , ("relates" :| [], [], newRelation)
    , ("can" :| ["be"], [], canBe)
    , ("is" :| ["either"], [], canBe)
    , ("is" :| ["listed"], [], inRulebook)
    , ("are" :| ["listed"], [], inRulebook)
    , ("is" :| ["not", "listed"], [], inRulebook)
    , ("are" :| ["not", "listed"], [], inRulebook)
    , ("has" :| [], [], verbAssertion toHave)
    , ("have" :| [], [], verbAssertion toHave)
    , ("is" :| [], [], verbAssertion toBe)
    , ("are" :| [], [], verbAssertion toBe)
    ]

toBe :: Verb
toBe = error "not implemented"

toHave :: Verb
toHave = error "not implemented"

data Verb = Verb

verbAssertion :: Verb -> Parser ExprLoc
verbAssertion = error "not implemented"

inRulebook :: Parser ExprLoc
inRulebook = error "not implemented"

canBe :: Parser ExprLoc
canBe = error "not implemented"

newRelation :: Parser ExprLoc
newRelation = error "not implemented"

newVerb :: Parser ExprLoc
newVerb = error "not implemented"

newAction :: Parser ExprLoc
newAction = error "not implemented"

newActivity :: Parser ExprLoc
newActivity = error "not implemented"

fileHandling :: Parser ExprLoc
fileHandling = error "not implemented"

thePluralOf :: Parser ExprLoc
thePluralOf = error "not implemented"

episode :: Parser ExprLoc
episode = error "not implemented"

definedBy :: Parser ExprLoc
definedBy = error "not implemented"

specifies :: Parser ExprLoc
specifies = error "not implemented"

sceneAnchoring :: Parser ExprLoc
sceneAnchoring = error "not implemented"

documentAt :: Parser ExprLoc
documentAt = error "not implemented"

debugging :: Parser ExprLoc
debugging = error "not implemented"

testWith :: Parser ExprLoc
testWith = error "not implemented"

understandAs :: Parser ExprLoc
understandAs = error "not implemented"

translatesIntoUnicode :: Parser ExprLoc
translatesIntoUnicode = error "not implemented"

updateAcc :: PhraseAccumulator -> Text -> PhraseAccumulator
updateAcc acc w = let
  innerLoop p' x xs = if
    x == w
      then
        case xs of
          [] -> Just $ Right p'
          (x' : xs') -> Just $ Left (p', x' :| xs')
      else
        Nothing
  newFindings = map (\p'@(Phrase _ (x :| xs) _ _) -> innerLoop p' x xs) (_phraseAccAll acc)
  updateCurrent = map (\(p', x :| xs) -> innerLoop p' x xs) (_phraseAccCurrent acc)
  parts = partitionEithers $ catMaybes (newFindings ++ updateCurrent)
  in
    acc
      { _phraseAccFound = _phraseAccFound acc <> snd parts
      , _phraseAccCurrent = fst parts
      }