{- |
Copyright: (c) 2022 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solitude (
module Relude
, module Optics
, module Relude.Extra.Bifunctor
, module Relude.Extra.Tuple
, module Formatting.Buildable
, module Text.Interpolation.Nyan
, isPrefixOf'
, caseM
, wrap
, composel
, isSuffixOf'
, surroundM
, (<$$>)
, prettyPrintList
, bothAnd
, Reversing(..)
, reversed
, universeSans -- UNDERTALE???
, (<<+~)
, (<<-~)
, eitherJoin
, thenATraverse
, (<$?>)
, MonadRS) where

import Relude
import Optics hiding (uncons)
import qualified Data.Text as T
import Relude.Extra.Bifunctor
import Relude.Extra.Tuple
import qualified Data.List.NonEmpty as NonEmpty
import Formatting.Buildable
import Data.List ((\\))
import Text.Interpolation.Nyan

  -- | Obtain a list of all members of a type universe, sans a finite list
universeSans
  :: Bounded x
  => Enum x
  => Ord x
  => [x]
  -> [x]
universeSans x = universe \\ x

class Reversing t where
  reversing :: t -> t

instance Reversing (NonEmpty a) where
  reversing = NonEmpty.reverse

reversed :: Reversing a => Iso' a a
reversed = involuted reversing

instance Reversing [a] where
  reversing = reverse

bothAnd ::
  a
  -> a
  -> (a -> Bool)
  -> Bool
bothAnd a1 a2 f = f a1 && f a2

-- | generalised version of `isPrefixOf` for when the lists are of different types
isPrefixOf' ::
  (a -> b -> Bool)
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

isSuffixOf'
  :: Text
  -> Text
  -> Bool
isSuffixOf' a b = T.toLower a `T.isSuffixOf` T.toLower b

surroundM
  :: Monad m
  => m a -- ^ how to set it up
  -> m b -- ^ what to do
  -> (a -> m c) -- ^ how to take it apart again
  -> m b
surroundM pre' doIt post = do
  p' <- pre'
  r <- doIt
  _ <- post p'
  return r

(<$$>)
  :: Functor f
  => Functor g
  => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$?>

(<$?>)
  :: (a -> Bool)
  -> Maybe a
  -> Bool
f <$?> m = maybe False f m

prettyPrintList :: [Text] -> Text
prettyPrintList [] = ""
prettyPrintList [x] = x
prettyPrintList [x, y] = x <> ", and " <> y
prettyPrintList (x:xs) = x <> ", " <> prettyPrintList xs


infixr 4 <<+~, <<-~

-- | Increment the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<+~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<+~) l b s = (s ^. l, s & l %~ (+b))
{-# INLINE (<<+~) #-}

-- | Decrement the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<-~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<-~) l b s = (s ^. l, s & l %~ (\x -> x - b))
{-# INLINE (<<-~) #-}

type MonadRS a m = (MonadReader a m, MonadState a m)


eitherJoin
  :: AffineTraversal' a f
  -> AffineTraversal' b f
  -> AffineTraversal' (Either a b) f
eitherJoin t1 t2 = (_Left % t1) `thenATraverse` (_Right % t2)

thenATraverse
  :: Is t1 An_AffineTraversal
  => Is t2 An_AffineTraversal
  => Optic t1 ix s s a b
  -> Optic t2 ix s s a b
  -> AffineTraversal s s a b
thenATraverse o1 o2 = atraversal
  ( \s -> case matching o1 s of
      Left _ -> matching o2 s
      Right f -> Right f
  )
  (\s b -> s & castOptic @An_AffineTraversal o1 .~ b
           & castOptic @An_AffineTraversal o2 .~ b
  )