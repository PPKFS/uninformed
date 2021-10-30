{- |
Copyright: (c) 2021 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <avery@chordify.net>

See README for more info
-}

module Uninformed.Sentencifier where

import Text.Megaparsec hiding (State)
import Data.Void
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Relude
import qualified Text.Megaparsec as MP

import qualified Uninformed.Lexer as Lexer
import qualified Data.Set as Set
import Data.List.Split (split, keepDelimsR, whenElt)
import Data.List (zipWith3)
import Control.Monad (foldM)
import Optics
import Optics.State.Operators

type Sentencifier = Parsec Void [Lexer.Lexeme]
data Sentence = Sentence [Lexer.Lexeme] | SourceFileChangeSentence | Table [Lexer.Lexeme]
type Paragraph = [Sentence]

--This is taken from 4/sent, starting at 4.5.

single' :: (MonadParsec e s m, Eq a, Token s ~ (a, b)) => a -> m (Token s)
single' y = satisfy (\x -> fst x == y)

someTillAndEnd :: (MonadParsec e s m) => m a -> m end -> m ([a], end)
someTillAndEnd p end = do
    a <- p
    r <- manyTillAndEnd p end
    pure (first (a :) r)

manyTillAndEnd :: (MonadParsec e s m) => m a -> m end -> m ([a], end)
manyTillAndEnd p end = do
    r <- (Left <$> end) <|> (Right <$> someTillAndEnd p end)
    pure $ either ([],) id r

oneOf' :: (MonadParsec e s m, Eq a, Token s ~ (a, b)) => [a] -> m (Token s)
oneOf' l = satisfy (\x -> fst x `elem` l)
data SentenceNode

type NodeBuilder a = ExceptT (State NodeState a)

data NodeState = NodeState
  { _currentSentence :: Int
  , _currentGlobalSentence :: Int
  , _isExtension :: Bool
  , _seenBegin :: Bool
  , _skipLevel :: Int
  , _currentHeadingLevel :: Int
  , _nodes :: [SentenceNode]
  }

makeLenses ''NodeState
paragraphsToTree
  :: [Paragraph]
  -> [SentenceNode]
  --foldM (b - a - mb ) - b - [a] - mb
paragraphsToTree p = mconcat $ evalState (mapM paragraphToSentenceNode p) blankNodeState

blankNodeState :: NodeState
blankNodeState = error "not implemented"

paragraphToSentenceNode
  :: Paragraph
  -> NodeBuilder [SentenceNode]
paragraphToSentenceNode p = mconcat <$> mapM (\case
        SourceFileChangeSentence -> changeOfSourceFile
        Table s -> error "not implemented"
        Sentence s -> makeSentenceNode s) p

changeOfSourceFile :: State NodeState [SentenceNode]
changeOfSourceFile = do
    let n = makeNode HeadingNode [UnparsedSentence, HeadingLevel 0]
    skipLevel .= -1
    --DECLARE_HEADING
    return []
    --make a new node
    --check any loose ends with regards to ends here

makeSentenceNode
  :: [Lexer.Lexeme]
  -> State NodeState [SentenceNode]
makeSentenceNode s = do
    hNode <- checkForExplicitHeader s
    beginEnd <- checkForBeginningOrEnding s
    sk <- checkForCurrentlySkipping
    if sk
    then
        return $ catMaybes [hNode, beginEnd]
    else
        return $ catMaybes [hNode, beginEnd]
    --if we're dealing with a bib node
    --deal with everything else


    --if it's the first sentence:
        --if it's a source file change:
            --implicit heading
        --detect any heading
        --detect begin/end
        --check heading, and potentially make a heading node
    --if we're right at the start of the source, maybe a bib node
    --add a sentence


groupIntoParagraphs :: [Sentence] -> [Paragraph]
groupIntoParagraphs = split (keepDelimsR $ whenElt sentenceEndsInPBreak)

sentenceEndsInPBreak :: Sentence -> Bool
sentenceEndsInPBreak (Table _) = True
sentenceEndsInPBreak SourceFileChangeSentence = True
sentenceEndsInPBreak (Sentence l) = fromMaybe False (viaNonEmpty (\ x -> fst (last x) == Lexer.ParagraphBreak) l)

lexemesToSentences :: Sentencifier [Sentence]
lexemesToSentences = manyTill parseSentence (single' Lexer.EndOfFile)

parseSentence :: Sentencifier Sentence
parseSentence = do
    --either we've started a new source file, which gives us a single sentence
    -- of a lone token
    (SourceFileChangeSentence <$ single' Lexer.SourceFileChange) <|>
    --it's a table
        parseTable <|>
        -- it's a bunch of words
            (do
                (a, b) <- manyTillAndEnd (parseTime <|> anySingle) sentenceEnding
                return $ Sentence $ a <> b)

-- | by simply handling times and making them into a new lexeme,
-- we avoid the whole colon ending problem of 4/sent.9
parseTime :: Sentencifier Lexer.Lexeme
parseTime = do
    i1 <- parseInt
    void $ single' (Lexer.PunctuationToken ':')
    i2 <- parseInt
    return (Lexer.TimeLexeme (fst i1) (fst i2), snd i1)
    where
        parseInt = token (\case
            (Lexer.OrdinaryWord v, b) -> ((, b) <$> readMaybe (T.unpack v))
            _ -> Nothing) Set.empty

parseTable :: Sentencifier Sentence
parseTable = do
    ts <- single' (Lexer.OrdinaryWord "Table")
    --and then it's everything until a paragraph break
    (a, b) <- manyTillAndEnd (parseTime <|> anySingle) (MP.some $ single' Lexer.ParagraphBreak)
    return $ Table $ [ts] <> a <> b

sentenceEnding :: Sentencifier [Lexer.Lexeme]
sentenceEnding = do
    --we concat them because we want to ignore empty sentences.
    --we only really care about the first punctuation mark, if one exists,
    --and then also if there are any paragraph breaks
    r <- mconcat <$> MP.some (
        (do
            o <- oneOf' [Lexer.PunctuationToken '.', Lexer.PunctuationToken ';', Lexer.ParagraphBreak]
            return [o]) <|>
        (do
            r1 <- satisfy (\(x, _) -> case x of
                    Lexer.PunctuationToken _ -> True
                    _ -> False)
            r2 <- single' Lexer.LiteralEnd
            return [r1, r2]))
    case r of
        (Lexer.LiteralEnd, p):xs -> return $ [(Lexer.LiteralEnd, p)] <> conjoinPunctuation (False, False, []) xs
        x -> return $ conjoinPunctuation (False, False, []) x

conjoinPunctuation :: (Bool, Bool, [Lexer.Lexeme]) -> [Lexer.Lexeme] -> [Lexer.Lexeme]
conjoinPunctuation (_, _, l) [] = l
conjoinPunctuation (p, pb, l) ((Lexer.PunctuationToken pt, pos):xs) = if p
    then conjoinPunctuation (p, pb, l) xs else conjoinPunctuation (True, pb, l ++ [(Lexer.PunctuationToken pt, pos)]) xs
conjoinPunctuation (p, pb, l) ((Lexer.ParagraphBreak, pos):xs) = if pb
    then conjoinPunctuation (p, pb, l) xs else conjoinPunctuation (p, pb, l ++ [(Lexer.ParagraphBreak, pos)]) xs
conjoinPunctuation (_, _, _) _ = []

colonEnding :: Sentencifier ()
colonEnding = void $ single' $ Lexer.PunctuationToken ':'