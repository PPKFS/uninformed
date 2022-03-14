{-# LANGUAGE FlexibleInstances #-}
module Uninformed.Parser.Expressions 
  ( Plain
  , ExprF(..)
  , ExprLoc
  , ExprPlain
  , WithLoc
  , SourceLocation(..)
  , WithLocF 
  
  , annotateLocation) where

import Uninformed.Prelude hiding (show)
import Uninformed.Headings.Types
import Uninformed.Extensions.Types
import Uninformed.NewVerb.Types
import Uninformed.Recursion
import Text.Megaparsec.Pos (SourcePos)
import Uninformed.Parser.Types
import Text.Megaparsec (getSourcePos)
import GHC.Show

data ExprF b =
  HeadingExpr Heading
  
  | ExtensionExpr (Extension b)
  | ExtensionHeaderExpr ExtensionHeader

  | NewVerbDeclarationExpr (NewVerbDeclaration b)
  | NewVerbDefinitionExpr NewVerbDefinition
  --so far, this only seems to be used for NVDecls but we'll see.
  | BinaryPredicateExpr BinaryPredicate

  deriving stock (Functor, Eq, Show)

type ExprLoc = WithLoc ExprF
type ExprPlain = Fix ExprF

newtype SourceLocation = SourceLocation
  { _parseNodeSpan :: (SourcePos, SourcePos)
  } deriving newtype (Eq, Show)

type WithLoc a = Fix (WithLocF a)
type Plain = Fix
type WithLocF = Compose ((,) SourceLocation)

instance Show ExprPlain where
  show = cata show

instance Eq ExprPlain where
  (==) (Fix a) (Fix b) = a == b

instance Show ExprLoc where
  show = cata sAnno where
    sAnno :: Compose ((,) SourceLocation) ExprF String -> String
    sAnno (Compose (r, g)) = show r <> show g

annotateLocation' :: Parser a -> Parser (SourceLocation, a)
annotateLocation' p = do
  begin <- getSourcePos
  res   <- p
  end   <- getSourcePos
  pure (SourceLocation (begin, end), res)

annotateLocation :: Parser (ExprF ExprLoc) -> Parser ExprLoc
annotateLocation = fmap (Fix . Compose) . annotateLocation'
