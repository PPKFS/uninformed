module Uninformed.Syntax.SyntaxTree where

import qualified Data.Vector as V

data NodeType = HeadingNode deriving stock (Show)

data Node a = Node
  { _nodePayload :: a
  , _nodeType :: NodeType
  , _nodeId :: Text
  } deriving stock Show

data SyntaxTree a = Leaf (Node a) | Branch (Node a) [SyntaxTree a] deriving stock (Show)

data Crumb a = Crumb (Node a) [SyntaxTree a] [SyntaxTree a] deriving stock (Show)

type Zipper a = (SyntaxTree a, [Crumb a])

up :: Zipper a -> Zipper a
up (n, []) = (n, [])
up (n, (Crumb node ls rs):bs) = (Branch node (V.concat [ls, one n, rs]), bs)

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