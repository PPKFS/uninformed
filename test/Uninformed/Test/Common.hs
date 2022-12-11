module Uninformed.Test.Common
  ( runGoldenSuite
  , readTestCases
  ) where
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Error.Diagnose
import System.Directory
import qualified Data.Map as Map
import Uninformed.Pipeline


missingCaseMap :: Map.Map String String
missingCaseMap = fromList
  [ --("Chapter12", "has some weird reason it doesn't lex but should be easy to fix")
  ]

missingCases :: String -> IO Bool
missingCases t = do
  let mbErr = Map.lookup t missingCaseMap
  case mbErr of
    Nothing -> pure True
    Just e -> print ("Skipping test " <> t <> " because it " <> e) >> pure False


readTestCases :: FilePath -> IO [(FilePath, Text)]
readTestCases fp = do
  let prfx = fp </> "Cases"
  fps <- liftIO (listDirectory prfx)
  fs <- mapM (fmap decodeUtf8 . readFileBS . (prfx </>)) fps
  filterM (\(fp', _) -> missingCases $ takeFileName . dropExtensions $ fp') (zip fps fs)



runGoldenSuite ::
  forall s.
  HasPipeline s
  => String
  -> String
  -> Proxy (s :: Stage)
  -> (PipelineOutput s -> Text)
  -> IO TestTree
runGoldenSuite grpName exemplarPrefix _ makeOutput = do
  fps <- liftIO (listDirectory (exemplarPrefix </> "Input"))
  files <- mapM (fmap decodeUtf8 . readFileBS . ((exemplarPrefix </> "Input") </>)) fps
  return $ testGroup grpName $
    flip map (zip fps files) $ \(fp, f) -> testCase fp $ do
        let res = runPipeline (Proxy @s) f
            output = case res of
              Left err' -> show $ prettyDiagnostic True 4 err'
              Right sf -> makeOutput sf
        error ""
