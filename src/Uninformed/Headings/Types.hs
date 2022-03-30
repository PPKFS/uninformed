{-# LANGUAGE TemplateHaskell #-}

module Uninformed.Headings.Types where

import Solitude
import Uninformed.Extensions.Types (ExtensionName)
import Optics

data Heading = Heading
  { _headingName :: HeadingName
  , _headingLevelNo :: Int
  , _headingInfo :: HeadingInfo
  } deriving stock (Eq, Show)

data HeadingName = HeadingName
  { _headingLevel :: HeadingLevel
  , _headingText :: Text
  }
  deriving stock (Eq, Show)

data HeadingLevel = File | Volume | Book | Part | Chapter | Section
  deriving stock (Eq, Enum, Ord, Show, Generic)

data ForRelease = ForReleaseOnly | NotForRelease
  deriving stock (Eq, Enum, Ord, Show, Generic)

data HeadingInfo = HeadingInfo
  { _headingForRelease :: Maybe ForRelease
  , _headingIsIndexed :: Bool
  , _headingOmitMaterial :: Bool
  , _headingUseWith :: Maybe UseWith
  , _headingInPlaceOf :: Maybe InPlaceOf
  } deriving stock (Eq, Show)

data InPlaceOf = InPlaceOf HeadingName ExtensionName deriving stock (Eq, Show)

data UseWith = UseWith ExtensionName | UseWithout ExtensionName deriving stock (Eq, Show)

defaultHeadingInfo :: HeadingInfo
defaultHeadingInfo = HeadingInfo Nothing True False Nothing Nothing

headerFluff :: [Char]
headerFluff = [',', '-', '~']

makeLenses ''Heading
makeLenses ''HeadingInfo
makeLenses ''HeadingName