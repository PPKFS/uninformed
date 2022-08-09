module Uninformed.Words.Vocabulary
  ( VocabularyEntry(..)
  , VocabType(..)
  , lowerVocabType

  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Set as Set
import qualified Data.Text as T

data VocabFlag = VocabFlag deriving stock (Eq, Show)

data VocabularyEntry = VocabularyEntry
  { _vocabFlags :: Set VocabFlag
  -- do we actually need these? who knows.
  , _vocabExemplar :: VocabType
  , _vocabRawExemplar :: VocabType
  -- we store the hash-codes for specifically-upper-cased/lower-cased words.
  , _vocabLowerCase :: Maybe Int
  , _vocabUpperCase :: Maybe Int
  , _vocabHashCode :: Int
  } deriving stock (Eq, Show)

type VocabMap = HashMap Int VocabularyEntry

identify :: VocabMap -> VocabType -> (VocabMap, VocabularyEntry)
identify vm vt = let
  hashStr = hash vt
  ve = makeVocabEntry hashStr vt in
    case HM.lookup hashStr vm of
      Nothing -> (HM.insert hashStr ve vm, ve)
      Just x -> (vm, x)

makeVocabEntry :: Int -> VocabType -> VocabularyEntry
makeVocabEntry vh vt = VocabularyEntry
  { _vocabFlags = Set.empty
  , _vocabExemplar = vt
  , _vocabRawExemplar = vt
  , _vocabLowerCase = Nothing
  , _vocabUpperCase = Nothing
  , _vocabHashCode = vh
  }

data VocabType =
  I6 Text
  | StringLit Text
  | OrdinaryWord Text
  | StringSub Text
  | ParagraphBreak deriving stock (Eq, Show, Ord, Generic)

instance Hashable VocabType

lowerVocabType :: VocabType -> VocabType
lowerVocabType = \case
  OrdinaryWord txt -> OrdinaryWord $ T.toLower txt
  x -> x
{-
instance ToText VocabType where
  toText = \case
    I6 txt -> "(-"<>txt<>"-)"
    StringLit txt -> _
    OrdinaryWord txt -> _
    StringSub txt -> _
    ParagraphBreak -> _
-}