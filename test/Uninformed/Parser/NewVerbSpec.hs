module Uninformed.Parser.NewVerbSpec (spec) where

import Uninformed.Prelude
import Test.Hspec

spec :: Spec
spec = pass
{-}
canParseNewVerb
  :: Text
  -> NewVerbDeclaration
  -> Expectation
canParseNewVerb = canParse (parseNewVerb <|> NewVerbDeclaration "" "" <$ string' "the verbiage")

spec :: Spec
spec = do
  describe "Implying a relation" $ do
    it "can parse an implying with no conjugation" $ do
      canParseNewVerb "The verb to be in implies the reversed containment relation." (NewVerbDeclaration "" "")
    it "test" $ do
      canParseNewVerb "the verbiage" (NewVerbDeclaration "" "")
-}