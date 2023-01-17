module Main where

import Uninformed.Prelude
import Test.Tasty
import qualified Uninformed.Test.Syntax.Zipper as Z
import qualified Uninformed.Test.Bulk as Bulk
import System.Directory
import System.FilePath

main :: IO ()
main = do
  let prfx = "test/Bulk/Cases"
  fps <- liftIO (listDirectory prfx)
  fs <- mapM (fmap decodeUtf8 . readFileBS . (prfx </>)) fps
  --s2 <- Sentences.spec2 lc
  defaultMain $ testGroup "Tests"
    [ Bulk.spec False $ zip fps fs
    , Z.spec
    ]