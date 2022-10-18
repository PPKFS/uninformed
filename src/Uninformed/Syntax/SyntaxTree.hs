{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Syntax.SyntaxTree where

import qualified Data.Vector as V
import qualified Data.Set as S
import Uninformed.Words.Lexer.Types

data NodeType = HeadingNode Int | RootNode deriving stock (Show)

data Node a = Node
  { _nodeAnnotations :: Set a
  , _nodeType :: NodeType
  , _nodeRaw :: Text
  , _nodeLocation :: SourceLocation
  , _nodeId :: Text
  } deriving stock Show

instance Eq (Node a) where
  (==) n1 n2 = _nodeId n1 == _nodeId n2

data SyntaxTree a = Leaf (Node a) | Branch (Node a) (V.Vector (SyntaxTree a)) deriving stock (Show, Eq)

data Crumb a = Crumb (Node a) (V.Vector (SyntaxTree a)) (V.Vector (SyntaxTree a)) deriving stock (Show)

newtype Zipper a = Zipper { unzipper :: (SyntaxTree a, [Crumb a]) }

instance Eq (Zipper a) where
  (==) (Zipper (t1, _)) (Zipper (t2, _)) = t1 == t2

focus ::
  Zipper a
  -> Node a
focus = getNode . fst . unzipper

getNode ::
  SyntaxTree a
  -> Node a
getNode (Leaf l) = l
getNode (Branch n _) = n

getHeadingLevel ::
  Zipper a
  -> Int
getHeadingLevel z = upWhile (\n -> case _nodeType . focus $ n of
  HeadingNode _ -> False
  _ -> True) z & focus & _nodeType & (\case
    HeadingNode n -> n
    _ -> error "impossible")

newSyntaxTree ::
  Text
  -> Zipper a
newSyntaxTree fn = Zipper (Leaf (Node
  { _nodeAnnotations = S.empty
  , _nodeType = RootNode
  , _nodeRaw = fn
  , _nodeId = "root"
  , _nodeLocation = SourceLocation (Just fn) Nothing (-1)
  }), [])

up :: Zipper a -> Zipper a
up (Zipper (n, [])) = Zipper (n, [])
up (Zipper (n, (Crumb node ls rs):bs)) = Zipper (Branch node (V.concat [ls, V.singleton n, rs]), bs)

upWhile :: 
  (Zipper a -> Bool)
  -> Zipper a
  -> Zipper a
upWhile f z = 
  if f z
    then 
      let upN = up z in 
        if upN == z then error "oops you hit the root"
        else upWhile f upN
    else
      z

left :: Zipper a -> Zipper a
left (Zipper (n, c)) = fromMaybe (Zipper (n, [])) $ do
  (Crumb node ls rs, bs) <- uncons c
  (ls', l) <- V.unsnoc ls
  return $ Zipper (l, Crumb node ls' (V.cons n rs):bs)

right :: Zipper a -> Zipper a
right (Zipper (n, c)) = fromMaybe (Zipper (n, [])) $ do
  (Crumb node ls rs, bs) <- uncons c
  (r, rs') <- V.uncons rs
  return $ Zipper (r, Crumb node (V.snoc ls n) rs':bs)

graftNodeChild ::
  Node a
  -> Zipper a
  -> Zipper a
graftNodeChild node = graftTreeChild (Leaf node)

graftTreeChild ::
  SyntaxTree a
  -> Zipper a
  -> Zipper a
graftTreeChild t z = error ""

graftTreeSibling ::
  SyntaxTree a
  -> Zipper a
  -> Zipper a
graftTreeSibling t z = error ""

makeLenses ''Node