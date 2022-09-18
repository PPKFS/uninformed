module Main where

import qualified Uninformed.Test.Words.Lexer as Lexer
import qualified Uninformed.Test.Syntax.Sentences as Sentences
import Test.Tasty
import System.Directory (listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  lc <- readTestCases "test/Uninformed/Test/"
  l <- Lexer.spec lc
  s <- Sentences.spec lc
  defaultMain $ testGroup "Tests"
    [ l, s
    ]

readTestCases :: FilePath -> IO [(FilePath, Text)]
readTestCases fp = do
  let prfx = fp </> "Cases"
  fps <- liftIO (listDirectory prfx)
  fs <- mapM (fmap decodeUtf8 . readFileBS . (prfx </>)) fps
  return $ zip fps fs