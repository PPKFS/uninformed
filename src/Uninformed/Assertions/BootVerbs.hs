{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Assertions.BootVerbs
  (

  ) where


import Uninformed.Prelude
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (singleton)

type VerbPhrase = Text
makeBuiltIn ::
  Monad m => m ()
makeBuiltIn = do
  --makeSpecialMeanings
  makeToBe
  --makeToMean

--conjugateVerb "be"
--conjugateVerb "mean"
makeToBe ::
  Monad m => m ()
makeToBe = do
  pass
{- }
auxiliaryHave :: VerbConjugation
auxiliaryHave = conjugateTabulation "auxiliary-have"

conjugateTabulation ::
  VerbPhrase
  -> VerbConjugation
conjugateTabulation v = go (blankConjugation v) v where
  go :: VerbConjugation -> VerbPhrase -> VerbConjugation
  go bl verb
    | "be" <- verb = conjugateToBe
    | "be able to" <- verb = conjugateToBeAbleTo
    | "be able to " `T.isPrefixOf` verb = conjugateToBeAbleToAuxiliary verb
    | verb `elem`
      [ "could"
      , "may"
      , "might"
      , "must"
      , "should"
      , "would"
      ] = conjugateModal verb
    | "auxiliary-have" <- verb = conjugateToHave bl
    | "do" <- verb = conjugateToDo bl
    | "'re" <- verb = conjugateContractedToBe
    | "'ve" <- verb = conjugateContractedToHave
    | "aren't" <- verb = conjugateArent
    | "can't" <- verb = conjugateCant
    | verb `elem`
      [ "don't"
      , "haven't"
      , "mayen't"
      , "mightn't"
      , "mustn't"
      , "wouldn't"
      , "couldn't"
      , "shouldn't"
      , "won't"
      ] = conjugateInformalNegatedModal verb
    | otherwise = conjugateRegularVerb verb bl

conjugateRegularVerb ::
  VerbPhrase
  -> VerbConjugation
  -> VerbConjugation
conjugateRegularVerb vp vc = vc &
  #presentParticiple .~ regularPresentParticiple vp

regularPresentParticipleTrie :: RootedPredTrie VerbPhrase (VerbPhrase -> VerbPhrase)
regularPresentParticipleTrie = execPTBuilderT $ error ""

regularPresentParticiple ::
  VerbPhrase
  -> VerbPhrase
regularPresentParticiple = error ""

data Voice = Active | Passive
  deriving stock ( Eq, Ord, Enum, Bounded, Show, Generic )

data Tense = Present | Past | Perfect | PastPerfect | Future
  deriving stock ( Eq, Ord, Enum, Bounded, Show, Generic )

data Sense = Positive | Negative
  deriving stock ( Eq, Ord, Enum, Bounded, Show, Generic )

data VerbConjugation = VerbConjugation
  { infinitive :: VerbPhrase
  , presentParticiple :: VerbPhrase
  , pastParticiple :: VerbPhrase
  , present :: VerbPhrase
  , past :: VerbPhrase
  , conjugations :: M.Map (Voice, Tense, Sense) VerbConjugationTabulation
  , toBeAuxiliary :: (Maybe VerbPhrase, Maybe VerbPhrase)
  } deriving stock ( Eq, Ord, Show, Generic )

blankConjugation :: VerbPhrase -> VerbConjugation
blankConjugation v = setConjugations Nothing Nothing Nothing (constantVpc v) $ VerbConjugation
  { infinitive = v
  , presentParticiple = v
  , pastParticiple = v
  , present = v
  , past = v
  , conjugations = M.empty
  , toBeAuxiliary = (Nothing, Nothing)
  }

data VerbConjugationTabulation = VerbConjugationTabulation
  { fps :: VerbPhrase
  , sps :: VerbPhrase
  , tps :: VerbPhrase
  , fpp :: VerbPhrase
  , spp :: VerbPhrase
  , tpp :: VerbPhrase
  } deriving stock ( Eq, Show, Ord, Generic )

instance Monoid VerbConjugationTabulation where
  mempty = constantVpc ""

constantVpc ::
  VerbPhrase
  -> VerbConjugationTabulation
constantVpc t = VerbConjugationTabulation t t t t t t

instance Semigroup VerbConjugationTabulation where
  (<>) v1 v2 = VerbConjugationTabulation
    { fps = fps v1 <> fps v2
    , sps = sps v1 <> sps v2
    , tps = tps v1 <> tps v2
    , fpp = fpp v1 <> fpp v2
    , spp = spp v1 <> spp v2
    , tpp = tpp v1 <> tpp v2
    }

conjugateToHave :: VerbConjugation -> VerbConjugation
conjugateToHave vc = vc &
  #presentParticiple .~ "having"
  & #pastParticiple .~ "had"
  & setConjugations (Just Active) (Just Present) (Just Positive) toHavePresent
  & setConjugations (Just Active) (Just Present) (Just Negative) (toHavePresent <> constantVpc " not" )
  & setConjugations (Just Active) (Just Past) (Just Positive) (constantVpc "had")
  & setConjugations (Just Active) (Just Past) (Just Negative) (constantVpc "had not")
  where
    toHavePresent = onlyTpsDifferent "have" "has"

conjugateToDo :: VerbConjugation -> VerbConjugation
conjugateToDo vc = vc &
  #presentParticiple .~ "doing"
  & #pastParticiple .~ "done"
  & setConjugations (Just Active) (Just Present) (Just Positive) toDoPresent
  & setConjugations (Just Active) (Just Present) (Just Negative) (toDoPresent <> constantVpc " not" )
  & setConjugations (Just Active) (Just Past) (Just Positive) (constantVpc "did")
  & setConjugations (Just Active) (Just Past) (Just Negative) (constantVpc "did not")
  & conjugateWithOtherVerb (Just Active) (Just Perfect) Nothing
      auxiliaryHave (Just Present) (\ah -> ah <> constantVpc " done")
  & conjugateWithOtherVerb (Just Active) (Just PastPerfect) Nothing
      auxiliaryHave (Just Present) (\ah -> ah <> constantVpc " done")
  & setConjugations (Just Active) (Just Future) (Just Positive) (constantVpc "will do")
  & setConjugations (Just Active) (Just Future) (Just Negative) (constantVpc "will not do")
  & setPassiveAuxiliary "done by"
  where
    toDoPresent = onlyTpsDifferent "do" "does"

onlyTpsDifferent :: VerbPhrase -> VerbPhrase -> VerbConjugationTabulation
onlyTpsDifferent v1 v2 = VerbConjugationTabulation v1 v1 v2 v1 v1 v1

setPassiveAuxiliary :: VerbPhrase -> VerbConjugation -> VerbConjugation
setPassiveAuxiliary vp vc = vc & #toBeAuxiliary % _2 ?~ vp

conjugateWithOtherVerb ::
  Maybe Voice
  -> Maybe Tense
  -> Maybe Sense
  -> VerbConjugation
  -> Maybe Tense
  -> (VerbConjugationTabulation -> VerbConjugationTabulation)
  -> VerbConjugation
  -> VerbConjugation
conjugateWithOtherVerb mbVoice mbTense mbSense otherVerb mbOtherVerbTense vpcf vp =
  let otherVerbText v t s = otherVerb ^. #conjugations % at (v, fromMaybe t mbOtherVerbTense, s) % non (error "you deleted a conjugation")
  in
  vp & #conjugations %~
    M.unionWith (\_ r -> r) (M.fromList
    [ ((v, t, s), vpcf (otherVerbText v t s) )
    | v <- maybe universe singleton mbVoice
    , t <- maybe universe singleton mbTense
    , s <- maybe universe singleton mbSense
    ])

setConjugations ::
  Maybe Voice
  -> Maybe Tense
  -> Maybe Sense
  -> VerbConjugationTabulation
  -> VerbConjugation
  -> VerbConjugation
setConjugations mbVoice mbTense mbSense vpcf vp = vp & #conjugations %~
  M.unionWith (\_ r -> r) (M.fromList
    [ ((v, t, s), vpcf)
    | v <- maybe universe singleton mbVoice
    , t <- maybe universe singleton mbTense
    , s <- maybe universe singleton mbSense
    ])
-}