module Uninformed.NewVerb.Types
  ( Participle(..)
  , BinaryPredicate(..)
  , Pronoun(..)
  , Tense(..)
  , VerbConjugation(..)
  , VerbUsage(..)
  , NewVerbDeclaration(..)
  , NewVerbDefinition(..)
  , VerbKind(..)
  , ImpliesOrMeans(..)
  , verbDefinitionFullName
  ) where

import Uninformed.Prelude
import qualified Data.Map as Map
import Data.Text.Display

data Participle = Present | Past deriving stock (Eq, Show)
data ImpliesOrMeans = Implies | Means  deriving stock (Eq, Show)

data NewVerbDeclaration r = NewVerbDeclaration 
  { _newVerbDefinition :: r
  , _impliesOrMeans :: ImpliesOrMeans
  , _nvRelation :: r
  } deriving stock (Eq, Show, Functor)

data NewVerbDefinition = NewVerbDefinition
  { _newVerbKind :: VerbKind
  , _newVerbName :: Text 
  , _newVerbConjugations :: Maybe VerbConjugationTable
  }  deriving stock (Show)

verbDefinitionFullName :: NewVerbDefinition -> Text
verbDefinitionFullName NewVerbDefinition{..} = show _newVerbKind <> _newVerbName

instance Eq NewVerbDefinition where
  (==) a b = verbDefinitionFullName a == verbDefinitionFullName b

data BinaryPredicate = BinaryPredicate deriving stock (Eq, Show)

-- he/she/it
data Pronoun = Singular | They deriving stock (Eq, Show)

data Tense = Is | Was | HasBeen | HadBeen deriving stock (Eq, Show)
data VerbKind = AbleTo | Prepositional | Regular deriving stock (Eq, Show)

data VerbConjugation = VerbConjugation
  { _conjugationPronoun :: Pronoun
  , _conjugationParticiple :: Maybe Participle
  , _conjugationText :: Text
  , _conjugationAdjectival :: Bool
  }  deriving stock (Eq, Show)

--todo: properly
type VerbConjugationTable = Map.Map Text (Set VerbUsage)

data VerbUsage = VerbUsage
  { _vuText :: Text -- ^ the name of this verb
  , _vuIsNegated :: Bool
  , _vuTense :: Tense
  , _vuMeaning :: ()
  , _vuUnderMainVerb :: ()
  , _vuSentenceIndex :: Int
  } deriving stock (Eq, Show)

instance Display VerbKind where
  displayBuilder AbleTo = "to be able to"
  displayBuilder Prepositional = "to be"
  displayBuilder Regular = "to"