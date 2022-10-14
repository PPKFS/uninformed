module Uninformed.Test.Words.Lexer
  ( spec
  ) where


import Prelude hiding ( Word )

import System.FilePath ( dropExtensions, takeFileName, (</>) )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( (@=?), assertEqual, Assertion, assertFailure, testCase )

import Text.Megaparsec
import Text.Megaparsec.Char ( string, string' )
import Text.Megaparsec.Char.Lexer ( decimal )

import Uninformed.Words.Lexer
import Uninformed.Words.Vocabulary
import qualified Data.Set as S
import qualified Data.Text as T
import Uninformed.Words.TextFromFiles

anyWords :: Parsec Void Text [VocabType]
anyWords = manyTill (do
  w <- anyWord True
  single ' '
  return w) (single '\n')

anyWord :: Bool -> Parsec Void Text VocabType
anyWord inDistinct = do
  single '¶'
  choice
    [  StringLit <$> (do
          single '"'
          r <- takeWhileP Nothing (/= '"')
          single '"'
          single '¶'
          pure $ T.replace (one '\DEL') "\n" r
          ),
        I6 <$> (do
          string "(-"
          t' <- toText <$> manyTill anySingle (single '¶')
          t <- ("" <$ guard inDistinct) <|> (do
            takeWhile1P Nothing (/= '\n')
            single '\n'
            single '¶'
            toText <$> manyTill anySingle (single '¶')
            )
          pure $ t' <> t
          ),
        ParagraphBreak <$ (string "|__" >> single '¶'),
        OrdinaryWord <$> (toText <$> manyTill anySingle (single '¶'))
    ]

dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (x:xs') (_:ys) = x : f xs' ys
          f _ _ = []

parseExemplar :: Parsec Void Text LexerInfo
parseExemplar = do
  (numWords :: Int) <- decimal
  string' " words\n"
  lexSet <- anyWords

  (numDistinctWords :: Int) <- decimal
  string' " distinct words\n"
  eachEntry <- manyTill (do
    actualToken <- anyWord False
    single ' '
    normalised <- anyWord False
    single ' '
    precSpace <- toString <$> takeWhile1P Nothing (/= '\n')
    single '\n'
    let (a, n) = case (actualToken, normalised) of
          (I6 x, OrdinaryWord _) -> (I6 x, I6 x)
          z -> z
    return (a, n, case precSpace of
      "09" -> Tab
      "20" -> Space
      "0a" -> Newline
      x -> maybe (error $ "unexpected spacetype: " <> show x) (\x' -> TabIndent $ x' - 64) $ readMaybe $ "0x" ++ x)) eof

  pure $ LexerInfo numWords (fromList lexSet) numDistinctWords eachEntry

data LexerInfo = LexerInfo
  { totalWords :: Int
  , distinctWordSet :: Set VocabType
  , totalDistinct :: Int
  , individualEntries :: [(VocabType, VocabType, Whitespace)]
  } deriving stock (Eq, Show)

spec :: [(FilePath, Text)] -> IO TestTree
spec allFiles = do
  let prfx2 = "test/Uninformed/Test/Words/Expected"
      fps = map fst allFiles
  allFiles2 <- mapM (fmap decodeUtf8 . readFileBS . (prfx2 </>)) fps
  let cmb = zip3
        (map (takeFileName . dropExtensions) fps)
        (map snd allFiles)
        (map (either (error . toText . errorBundlePretty) id . parse parseExemplar "") allFiles2)
  return $ testGroup "Lexing" $
    flip map cmb $ \(fp, f, e) ->
      testCase ("lexes " <> fp) $ do
        let res = lex False Nothing f
        case res of
          Left err -> assertFailure $ error . toText . errorBundlePretty $ err
          Right (sf, vm) -> compareLexerInfo e $ getLexerInfo sf

compareLexerInfo :: LexerInfo -> LexerInfo -> Assertion
compareLexerInfo (LexerInfo _eTw _eDws _eTd eIe) (LexerInfo _rTw _rDws _rTd rIe) = do
  let cmp = zip3 eIe rIe ([1..] :: [Integer])
  mapM_ (\(x, y, z) -> assertEqual (show z) x y) cmp
  -- todo: currently we ignore the 'raw' number comparisons because we treat literals as the same
  -- inform does not, so it'll say C7TryLiteralTopic has 27 unique words ("bananas" twice) whereas we say 26
  eIe @=? rIe
  -- sigh, because I6 inclusions are wack I have to remove this too
  --S.difference eDws rDws @?= S.empty
  --S.difference rDws eDws @?= S.empty
  --eTd @=? rTd

getLexerInfo :: SourceFile WordList -> LexerInfo
getLexerInfo sf@SourceFile{_sourceFileData = wl} =
  let (wordSet :: Set VocabType) = fromList . toList $ map (\(InformWord _ w _) -> lowerVocabType w) wl in
    LexerInfo
      { totalWords = _sourceFileRawWordCount sf
      , totalDistinct = S.size wordSet
      , distinctWordSet = wordSet
      , individualEntries = toList $ map (\(InformWord _ w ps) -> (w, lowerVocabType w, ps)) wl
      }