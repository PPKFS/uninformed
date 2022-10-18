{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Syntax.SyntaxTree where

import qualified Data.Vector as V
import qualified Data.Set as S
import Uninformed.Words.Lexer.Types
import qualified Data.Vector.NonEmpty as VNE

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

data SyntaxTree a = Leaf (Node a) | Branch (Node a) (VNE.NonEmptyVector (SyntaxTree a)) deriving stock (Show, Eq)

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

consVector ::
  V.Vector a
  -> VNE.NonEmptyVector a
  -> VNE.NonEmptyVector a
consVector v vn = case VNE.fromVector v of
  Nothing -> vn
  Just x -> x VNE.++ vn

up ::
  Zipper a
  -> Zipper a
up (Zipper (_, [])) = error "cannot go up from the root."
up (Zipper (n, (Crumb node ls rs):bs)) = Zipper (Branch node (consVector ls (VNE.consV n rs)), bs)

down ::
  Zipper a
  -> Zipper a
down (Zipper (Leaf _, _)) = error "cannot go down from a leaf."
down (Zipper (Branch a vs, cs)) = let (h, hs) = VNE.uncons vs in Zipper (h, Crumb a V.empty hs:cs)

safeUp ::
  Zipper a
  -> Maybe (Zipper a)
safeUp (Zipper (_, [])) = Nothing
safeUp z = Just (up z)

-- either look for a condition at the current level, and then extract some info
-- if not, then try to move up and recurse
safeFindUp ::
  (Node a -> Maybe b)
  -> (b -> c)
  -> Zipper a
  -> Maybe c
safeFindUp f cont z = fmap cont (f . focus $ z) <|> (safeUp z >>= safeFindUp f cont)

-- whilst some condition holds, keep going up. stop the zipper at the first instance of a broken condition.
upWhile ::
  (Zipper a -> Bool)
  -> Zipper a
  -> Zipper a
upWhile f z = if f z then upWhile f (up z) else z

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

downFurthestRight :: Zipper a -> Zipper a
downFurthestRight (Zipper (Leaf _, _)) = error "cannot go down from a leaf."
downFurthestRight (Zipper (Branch a vs, cs)) = let (hs, h) = VNE.unsnoc vs in Zipper (h, Crumb a hs V.empty:cs)

graftNodeChild ::
  Node a
  -> Zipper a
  -> Zipper a
graftNodeChild node = graftTreeChild (Leaf node)

graftTreeChild ::
  SyntaxTree a
  -> Zipper a
  -> Zipper a
graftTreeChild t (Zipper (Leaf a, cs)) = down (Zipper (Branch a (VNE.singleton t), cs))
graftTreeChild t (Zipper (Branch a ns, cs)) = downFurthestRight (Zipper (Branch a (VNE.snoc ns t), cs))

graftTreeSibling ::
  SyntaxTree a
  -> Zipper a
  -> Zipper a
graftTreeSibling t z = error ""

makeLenses ''Node