module Uninformed.Test.Syntax.Zipper
  ( spec
  ) where
import Test.Tasty
import Uninformed.Words.Lexer
import Test.Tasty.HUnit
import qualified Data.Text as T
import Uninformed.Test.Common
import Uninformed.Syntax.SyntaxTree

checkNodeRaw :: Zipper a -> Text -> Assertion
checkNodeRaw z t =(focus z & _nodeRaw) @=? t
addChild :: Text -> Zipper a -> Zipper a
addChild t = graftNodeChild (blankNode t "nn") 
spec :: TestTree
spec = testCase "Zipper" $ do
    let z = newSyntaxTree @Int "0"
    -- it is focused on the root
    checkNodeRaw z "0"
    let z1 = addChild "1" z & addChild "1.1" & addChild "1.1.1"
    checkNodeRaw z1 "1.1.1"
    let z2 = up z1
    checkNodeRaw z2 "1.1"
    let z3 = addChild "1.1.2" z2
    checkNodeRaw z3 "1.1.2"
    let z4 = left z3
    checkNodeRaw z4 "1.1.1"
    
