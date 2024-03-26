module Uninformed.Vocabulary
  ( VocabMap
  , makeVocabularyEntryntry
  , identify
  , VocabularyEntry(..)
  ) where

import Uninformed.Prelude
import qualified Data.HashMap.Strict as HM
import Data.Hashable ( Hashable(..) )
import qualified Data.Set as Set
import Uninformed.Word

data VocabFlag = VocabFlag deriving stock (Eq, Show)

data VocabularyEntry = VocabularyEntry
  { flags :: Set VocabFlag
  -- do we actually need these? who knows.
  , exemplar :: Word
  , rawExemplar :: Word
  -- we store the hash-codes for specifically-upper-cased/lower-cased words.
  , lowerCaseHash :: Maybe Int
  , upperCaseHash :: Maybe Int
  , hashCode :: Int
  } deriving stock (Eq, Show)

type VocabMap = HashMap Int VocabularyEntry

identify :: Word -> VocabMap -> (VocabularyEntry, VocabMap)
identify vt vm = let
  hashStr = hash vt
  ve = makeVocabularyEntryntry hashStr vt in
    case HM.lookup hashStr vm of
      Nothing -> (ve, HM.insert hashStr ve vm)
      Just x -> (x, vm)

makeVocabularyEntryntry :: Int -> Word -> VocabularyEntry
makeVocabularyEntryntry vh vt = VocabularyEntry
  { flags = Set.empty
  , exemplar = vt
  , rawExemplar = vt
  , lowerCaseHash = Nothing
  , upperCaseHash = Nothing
  , hashCode = vh
  }
