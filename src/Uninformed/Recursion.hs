module Uninformed.Recursion
  ( Fix(..)
  , cata
  , strip

  ) where

import Solitude

newtype Fix f = Fix { unFix :: f (Fix f) }

type Algebra f a = f a -> a 

cata :: Functor f => Algebra f a -> Fix f -> a 
cata f = f . fmap (cata f) . unFix

strip :: 
  Functor f
  => Fix (Compose ((,) a) f)
  -> Fix f
strip = cata (\(Compose (_, f)) -> Fix f)
