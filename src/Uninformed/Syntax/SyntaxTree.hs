module Uninformed.Syntax.SyntaxTree where

import qualified Data.Vector as V

data NodeType = HeadingNode deriving stock (Show)

data Node a = Node
  { _nodePayload :: a
  , _nodeType :: NodeType
  , _nodeId :: Text
  } deriving stock Show

data SyntaxTree a = Leaf (Node a) | Branch (Node a) (V.Vector (SyntaxTree a)) deriving stock (Show)

data Crumb a = Crumb (Node a) (V.Vector (SyntaxTree a)) (V.Vector (SyntaxTree a)) deriving stock (Show)

type Zipper a = (SyntaxTree a, [Crumb a])

up :: Zipper a -> Zipper a
up (n, []) = (n, [])
up (n, (Crumb node ls rs):bs) = (Branch node (V.concat [ls, V.singleton n, rs]), bs)

left :: Zipper a -> Zipper a
left (n, c) = fromMaybe (n, []) $ do
  ((Crumb node ls rs), bs) <- uncons c
  (ls', l) <- V.unsnoc ls
  return (l, (Crumb node ls' $ V.cons n rs):bs)

right :: Zipper a -> Zipper a
right (n, c) = fromMaybe (n, []) $ do
  ((Crumb node ls rs), bs) <- uncons c
  (r, rs') <- V.uncons rs
  return (r, (Crumb node (V.snoc ls n) rs'):bs)

graftSentence ::
  Node a
  -> Zipper a
  -> Zipper a
graftSentence node = graftTree (Leaf node)

graftTree ::
  SyntaxTree a
  -> Zipper a
  -> Zipper a
graftTree = error ""