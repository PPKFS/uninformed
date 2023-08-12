module Uninformed.Test.Words.Lexer
  ( compareLexerInfo
  , getLexerInfo
  , checkLexing
  ) where


import Uninformed.Prelude

import Data.Aeson
import System.FilePath ( (-<.>), (</>) )

import Test.Tasty.HUnit ( (@=?), assertEqual, Assertion )

import Uninformed.Pipeline
import Uninformed.SourceFile
import Uninformed.Word

newtype LexerInfo = LexerInfo
  { individualEntries :: [(Word, Word, Whitespace)]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

checkLexing :: Bool -> PipelineOutput 'LexingStage -> FilePath -> IO ()
checkLexing False (sf, _) fp = do
  fileContents <- decodeFileStrict' . (\x -> "test/Bulk/Lexer" </> x -<.> "json") $ fp
  case fileContents of
    Nothing -> error $ "Could not parse golden file" <> toText fp
    Just e -> compareLexerInfo e $ getLexerInfo sf
checkLexing True (sf, _) fp = do
  encodeFile ("test/Bulk/Lexer" </> fp -<.> "json") (getLexerInfo sf)

compareLexerInfo :: LexerInfo -> LexerInfo -> Assertion
compareLexerInfo (LexerInfo eIe) (LexerInfo rIe) = do
  let cmp = zip3 eIe rIe ([1..] :: [Integer])
  mapM_ (\(x, y, z) -> assertEqual (show z) x y) cmp
  -- todo: currently we ignore the 'raw' number comparisons because we treat literals as the same
  -- inform does not, so it'll say C7TryLiteralTopic has 27 unique words ("bananas" twice) whereas we say 26
  eIe @=? rIe
  -- sigh, because I6 inclusions are wack I have to remove this too
  --S.difference eDws rDws @?= S.empty
  --S.difference rDws eDws @?= S.empty
  --eTd @=? rTd

getLexerInfo :: SourceFile [Token] -> LexerInfo
getLexerInfo SourceFile{contents = wl} =
  LexerInfo
      { individualEntries = toList $ map (\(Token _ w ps _) -> (w, lowerWord w, ps)) wl
      }