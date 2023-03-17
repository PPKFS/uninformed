module Uninformed.Prelude
  ( module Solitude
  , zipWithPadding
  , longestSpan
  , isNewline
  , isHspace
  ) where

import Solitude hiding ( Word )
import Data.Char (isSpace)

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

longestSpan :: (a -> Bool) -> [a] -> Int
longestSpan f lst = fst $ foldl'
  (\(best, cur) v -> if f v then (if cur >= best then cur+1 else best, cur+1) else (best, 0)) (0, 0) lst

isNewline :: Char -> Bool
isNewline x = x `elem` ['\n', '\DEL', '\r']

isHspace :: Char -> Bool
isHspace x = isSpace x && not (isNewline x)