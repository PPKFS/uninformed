module Uninformed.Prelude
  ( module Solitude
  , zipWithPadding
  , longestSpan
  , isNewline
  , isHspace
  , whileM'
  , whileM_
  ) where

import Solitude hiding ( Word )
import Data.Char (isSpace)

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ [] ys = map (a, ) ys
zipWithPadding _ b xs [] = map (, b) xs

longestSpan :: (a -> Bool) -> [a] -> Int
longestSpan f lst = fst $ foldl'
  (\(best, cur) v -> if f v then (if cur >= best then cur+1 else best, cur+1) else (best, 0)) (0, 0) lst

isNewline :: Char -> Bool
isNewline x = x `elem` ['\n', '\DEL', '\r']

isHspace :: Char -> Bool
isHspace x = isSpace x && not (isNewline x)

whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x'  <- f
                        xs <- go
                        return (return x' `mplus` xs)
                else return mzero

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x $ f >> go