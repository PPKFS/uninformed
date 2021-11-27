module Uninformed.Prelude
( module Relude
, isPrefixOf'
, caseM
, wrap
, composel
, removeDashOrWhitespace
, isSuffixOf'
) where

import Relude
import qualified Data.Text as T
import Data.Char (isSpace)

-- | generalised version of `isPrefixOf` for when the lists are of different types
isPrefixOf'
  :: (a -> b -> Bool)
  -> [a]
  -> [b]
  -> Bool
isPrefixOf' _ [] _ = True
isPrefixOf' _ (_:_) [] = False
isPrefixOf' eq (l:ls) (x:xs) = eq l x && isPrefixOf' eq ls xs

caseM
  :: Monad m
  => [MaybeT m a]
  -> m a
  -> m a
caseM cases fallback = runMaybeT (asum cases) >>= maybe fallback pure

wrap
  :: Semigroup a 
  => a
  -> a
  -> a
wrap a b = a <> b <> a

composel
  :: Foldable f 
  => f (a -> a) 
  -> a 
  -> a
composel = foldl' (.) id

removeDashOrWhitespace
  :: Text
  -> Text
removeDashOrWhitespace = T.dropAround (\x -> isSpace x || x == '-')

isSuffixOf'
  :: Text 
  -> Text 
  -> Bool
isSuffixOf' a b = T.toLower a `T.isSuffixOf` T.toLower b