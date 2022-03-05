module Uninformed.NewVerb.Types where

import Uninformed.Prelude
import qualified Data.Map as Map
import Data.Text.Display

data Participle = Present | Past

data NewVerbDeclaration r = NewVerbDeclaration Text Text deriving stock (Eq, Show, Functor)

data BinaryPredicate = BinaryPredicate

-- he/she/it
data Pronoun = Singular | They

data Tense = Is | Was | HasBeen | HadBeen
data VerbKind = AbleTo | Prepositional | Regular 

data VerbConjugation = VerbConjugation
  { _conjugationPronoun :: Pronoun
  , _conjugationParticiple :: Maybe Participle
  , _conjugationText :: Text
  , _conjugationAdjectival :: Bool
  }

--todo: properly
type VerbConjugationTable = Map.Map Text (Set VerbUsage)

data VerbUsage = VerbUsage
  { _vuText :: Text -- ^ the name of this verb
  , _vuIsNegated :: Bool
  , _vuTense :: Tense
  , _vuMeaning :: ()
  , _vuUnderMainVerb :: ()
  , _vuSentenceIndex :: Int
  }

instance Display VerbKind where
  displayBuilder AbleTo = "to be able to"
  displayBuilder Prepositional = "to be"
  displayBuilder Regular = "to"