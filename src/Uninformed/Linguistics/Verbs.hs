module Uninformed.Linguistics.Verbs where

data Verb = Verb
  { _verbConjugation :: Conjugation
  , _verbFirstForm :: VerbForm
  , _verbBaseForm :: VerbForm
  , _verbStockItem :: LinguisticStockItem
  --, verbCompilationData :: VerbCompilationData
  }