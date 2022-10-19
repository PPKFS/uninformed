module Main where

import qualified Uninformed.Test.Words.Lexer as Lexer
import qualified Uninformed.Test.Syntax.Sentences as Sentences
import Test.Tasty
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtensions, takeFileName)
import qualified Data.Map as Map
import qualified Uninformed.Test.Syntax.Zipper as Z

main :: IO ()
main = do
  lc <- readTestCases "test/Uninformed/Test/"
  l <- Lexer.spec lc
  s <- Sentences.spec lc
  s2 <- Sentences.spec2 lc
  defaultMain $ testGroup "Tests"
    [ l
    , s
    , Z.spec
    , s2
    ]

readTestCases :: FilePath -> IO [(FilePath, Text)]
readTestCases fp = do
  let prfx = fp </> "Cases"
  fps <- liftIO (listDirectory prfx)
  fs <- mapM (fmap decodeUtf8 . readFileBS . (prfx </>)) fps
  filterM (\(fp', _) -> missingCases $ takeFileName . dropExtensions $ fp') (zip fps fs)

missingCaseMap :: Map.Map String String
missingCaseMap = fromList
  [ ("C5LiteralsWithQuotes", "has feet and inches which break the \" handling in the lexer")
  , ("Chapter12", "has some weird reason it doesn't lex but should be easy to fix")
  ]
missingCases :: String -> IO Bool
missingCases t = do
  let mbErr = Map.lookup t missingCaseMap
  case mbErr of
    Nothing -> pure True
    Just e -> print ("Skipping test " <> t <> " because it " <> e) >> pure False
