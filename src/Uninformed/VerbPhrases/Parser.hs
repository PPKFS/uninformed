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
    zipWith (\x (y, z) -> Phrase x y z) [1..] [
      ("translates" :| ["into", "unicode"], [])
    , ("as" :| [], ["Understand"])
    , ("with" :| [], ["Test"])
    , ("in" :| ["the", "debugging"], ["Include"])
    , ("from" :| ["the", "debugging"], ["Omit"])
    , ("at" :| [], ["Document"])
    , ("begins" :| ["when"], [])
    , ("ends" :| ["when"], [])
    , ("when" :| [], []) --iffy because we need an "ends..." in here too but with possible other stuff
    , ("specifies" :| [], [])
    , ("are" :| ["defined", "by"], [])
    , ("story" :| ["is", "episode"], ["The", "story", "is", "episode"])
    , ("is" :| [], ["The", "plural", "of"])
    , ("is" :| ["the", "file"], ["Figure"])
    , ("is" :| ["the", "file"], ["Sound", "effect"])
    , ("is" :| ["the", "file"], ["Sound"])
    --missing the "text/binary" stuff here too
    , ("is" :| ["an", "activity"], [])
    , ("is" :| ["an", "action"], [])
    , ("implies" :| [], ["The", "verb", "to"])
    , ("relates" :| [], [])
    , ("can" :| ["be"], [])
    , ("is" :| ["either"], [])
    , ("is" :| ["listed"], [])
    , ("are" :| ["listed"], [])
    , ("is" :| ["not", "listed"], [])
    , ("are" :| ["not", "listed"], [])
    , ("has" :| [], [])
    , ("have" :| [], [])
    , ("is" :| [], [])
    , ("are" :| [], [])
    ]

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
  newFindings = map (\p'@(Phrase _ (x :| xs) _) -> innerLoop p' x xs) (_phraseAccAll acc)
  updateCurrent = map (\(p', x :| xs) -> innerLoop p' x xs) (_phraseAccCurrent acc)
  parts = partitionEithers $ catMaybes (newFindings ++ updateCurrent)
  in
    acc
      { _phraseAccFound = _phraseAccFound acc <> snd parts
      , _phraseAccCurrent = fst parts
      }


pastTenseError :: Parser a0
pastTenseError = fail "not implemented"

useOrRelease :: Parser a0
useOrRelease = fail "not implemented"

haskellTranslation :: Parser a0
haskellTranslation = fail "not implemented"

assertionAboutThings :: Parser a0
assertionAboutThings = fail "not implemented"

rulePositioning :: Parser a0
rulePositioning = fail "not implemented"

propertyDeclaration :: Parser a0
propertyDeclaration = fail "not implemented"

verbOrRelationDeclaration :: Parser a0
verbOrRelationDeclaration = fail "not implemented"

actionOrActivity :: Parser a0
actionOrActivity = fail "not implemented"

resourceDeclaring :: Parser a0
resourceDeclaring = fail "not implemented"

newNotation :: Parser a0
newNotation = fail "not implemented"

sceneAnchoring :: Parser a0
sceneAnchoring = fail "not implemented"

debuggingLog :: Parser a0
debuggingLog = fail "not implemented"

understandOrTest :: Parser a0
understandOrTest = fail "not implemented"

