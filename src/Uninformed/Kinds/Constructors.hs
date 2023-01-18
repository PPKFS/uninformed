module Uninformed.Kinds.Constructors where

import Uninformed.Prelude

{-
So I think I've got it
a 'base' kind is *; arity 0
a 'proper constructor' is * -> * or * -> * -> *; arity 1 or 2
a protocol is * -> Constraint
punctuation is...a proper constructor which is weird?
-}

data KindConstructor (arity :: Nat) = KindConstructor
  { name :: Text
  , kind :: ConstructorKind
  , variance :: VS.Vector arity Variance


  }