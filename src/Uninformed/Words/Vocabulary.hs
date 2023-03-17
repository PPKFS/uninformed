module Uninformed.Words.Vocabulary
  ( VocabMap
  , makeVocabEntry
  , identify
  ) where

import Uninformed.Prelude
import qualified Data.HashMap.Strict as HM
import Data.Hashable ( Hashable(..) )
import qualified Data.Set as Set
import Uninformed.Word

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

identify :: VocabType -> VocabMap -> (VocabularyEntry, VocabMap)
identify vt vm = let
  hashStr = hash vt
  ve = makeVocabEntry hashStr vt in
    case HM.lookup hashStr vm of
      Nothing -> (ve, HM.insert hashStr ve vm)
      Just x -> (x, vm)

makeVocabEntry :: Int -> VocabType -> VocabularyEntry
makeVocabEntry vh vt = VocabularyEntry
  { _vocabFlags = Set.empty
  , _vocabExemplar = vt
  , _vocabRawExemplar = vt
  , _vocabLowerCase = Nothing
  , _vocabUpperCase = Nothing
  , _vocabHashCode = vh
  }
