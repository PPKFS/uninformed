module Uninformed.Sentences.Break
  ( breakIntoSentences
  , defaultSentenceBreakerState
  ) where

import Uninformed.Prelude hiding ( (|>) )
import Uninformed.Word
import qualified Data.Text as T
import Data.Char (isUpper, isPunctuation)
import Uninformed.Syntax.Sentences
import Uninformed.Words.TextFromFiles
import Error.Diagnose
import Optics.State (use)
import Optics.State.Operators
import qualified Control.Monad.State.Strict as S
import Data.List (isSuffixOf)

data ExtensionPosition = NotInExtension | BeforeBegins | InExtension | AfterEnd

data SentenceBreakerState = SentenceBreakerState
  { sourceFile :: SourceFile [Word]
  , extensionPosition :: ExtensionPosition
  , skippingAtLevel :: Maybe Int
  , inRuleMode :: Bool
  , inTableMode :: Bool
  , inDialogueMode :: Bool
  , currentStream :: [(Word, Word, Word)]
  , sentences :: [Sentence]
  } deriving stock (Generic)

defaultSentenceBreakerState ::
  SourceFile [Word]
  -> SentenceBreakerState
defaultSentenceBreakerState sf = let cs = sf ^. #sourceFileData in
  SentenceBreakerState
  { sourceFile = sf
  , extensionPosition = NotInExtension
  , skippingAtLevel = Nothing
  , inRuleMode = False
  , inTableMode = False
  , inDialogueMode = False
  , currentStream = zip3 (blankWord:cs) cs (snoc (fromMaybe [] (viaNonEmpty tail cs)) blankWord )
  , sentences = []
  }

type Parser m = MonadState SentenceBreakerState m

type StructuredError = Diagnostic Text
type PipelineStage i o = i -> Either StructuredError o

breakIntoSentences ::
  Parser m
  => m [Sentence]
breakIntoSentences = do
  considerTableMode
  considerDialogueMode
  -- drop any excess pbreaks
  #currentStream %= dropWhile ((ParagraphBreak ==) . view (_2 % #word))
  s <- lookForSentenceBreak
  -- #currentStream %= drop ((1 + length stop +) $ maybe 0 (length . unSentence) s)
  case s of
    Nothing -> use $ #sentences % reversed
    Just s' -> do
      #sentences %= (s':)
      breakIntoSentences

considerTableMode ::
  Parser m
  => m ()
considerTableMode = do
  (cs :: Maybe Word) <- S.gets $ preview $ #currentStream % ix 0 % _2
  -- we don't actually need the full "structural sentence" machinery for table
  -- checking.
  #inTableMode %= (\tm -> maybe tm (matchWord (== OrdinaryWord "table")) cs)

considerDialogueMode ::
  Parser m
  => m ()
considerDialogueMode = do
  csFull <- use #currentStream
  let (cs :: Maybe Word) = csFull ^? ix 0 % _2
  -- same with dialogue.
    -- ...however we do need a fair amount to get out of dialogue mode
  #inDialogueMode %= (\dm ->
    maybe dm (\justCs ->
      -- if we have a 'section'...
      if matchWord (== OrdinaryWord "Section") justCs
      then
        -- see if there are brackets with 'dialogue' or 'dialog' in before the next pbreak
        checkSection (map (word . view _2) csFull) else
        --if we are in dialogue mode, matching a header takes us out. otherwise, keep it the same
        not (dm && matchWord
        (\vt -> any (\f -> f vt)
          [ (== OrdinaryWord "Section")
          , (== OrdinaryWord "chapter")
          , (== OrdinaryWord "book")
          , (== OrdinaryWord "volume")
          , (== OrdinaryWord "part")] ) justCs) && dm) cs)

checkSection :: [VocabType] -> Bool
checkSection csFull = let thisLine = takeWhile (/= ParagraphBreak) csFull
  in
  ([OpenParenthesis, OrdinaryWord "dialogue", CloseParenthesis] `isSuffixOf` thisLine)
  ||
  ([OpenParenthesis, OrdinaryWord "dialog", CloseParenthesis] `isSuffixOf` thisLine)


whileM' :: (Monad m, MonadPlus f) => m Bool -> m a -> m (f a)
whileM' p f = go
    where go = do
            x <- p
            if x
                then do
                        x'  <- f
                        xs <- go
                        return (return x' `mplus` xs)
                else return mzero

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            when x $ f >> go

considerQuotedPunctuation :: Bool -> VocabType -> VocabType -> Bool
considerQuotedPunctuation isInTable curr next = not isInTable && endsInPunctuation curr && isUppercaseWord next

isUppercaseWord :: VocabType -> Bool
isUppercaseWord (OrdinaryWord w') = maybe False (isUpper . fst) $ T.uncons w'
isUppercaseWord _ = False

endsInPunctuation :: VocabType -> Bool
endsInPunctuation (StringLit w') = maybe False (isPunctuation . snd) $ T.unsnoc w'
endsInPunctuation _ = False

notAtAStop ::
  Parser m
  => m Bool
notAtAStop = do
  cs <- use #currentStream
  isAtTable <- use #inTableMode
  isInDialogue <- use #inDialogueMode
  maybe (pure False) (\(prev, curr, nxt) ->
    pure $ not $ flip matchWord curr $
      \case
      -- a pbreak is always a stop, but the others are only stops if we are NOT in dialogue mode.
        ParagraphBreak -> True
        Period -> not isInDialogue
        Semicolon -> not isInDialogue
        Colon ->  not isInDialogue && considerColonDivision prev nxt
        _ ->  not isInDialogue && considerQuotedPunctuation isAtTable (word curr) (word nxt)) (cs ^? ix 0)

findNextStops ::
  Parser m
  => Word
  -> m Bool
findNextStops stopChar = do
  h <- S.gets (preview $ #currentStream % ix 0)
  case h of
    Nothing -> pure False
    Just (_, h', _) -> pure $ matchWord
        (\w' -> case (word stopChar, w') of
          (Colon, ParagraphBreak) -> error "colon at end of paragraph"
          (_, ParagraphBreak) -> True
          --let's ignore dialogue mode for now.
          (Colon, Period) -> error "colon at end of sentence"
          (Semicolon, Period) -> error "semicolon at end of sentence"
          (_, Period) -> True
          (Colon, Semicolon) -> error "semicolon after colon"
          (Period, Semicolon) -> error "semicolon after period"
          (_, Semicolon) -> True
          _ -> False
        ) h'

lookForSentenceBreak ::
  Parser m
  => m (Maybe Sentence)
lookForSentenceBreak = do
  -- grab everything until we encounter a stop
  sentenceWithoutStop <- fmap catMaybes $ whileM' notAtAStop $ do
    (\x -> x ^? ix 0 % _2) <$> (#currentStream <<%= drop 1)
  -- and then also grab whatever it was that we stopped at
  stop <- preview (to listToMaybe % _Just % _2) <$> (#currentStream <<%= drop 1)
  case (sentenceWithoutStop, stop) of
    -- all out
    ([], Nothing) -> pure Nothing
    -- this happens when the sentence contains its own stopchar and also it's eof.
    (x:xs, Nothing) -> pure $ Just $ Sentence $ x :| xs
    (x, Just s) -> do
      _stops <- whileM_ (findNextStops s) $ do
        (\x' -> x' ^? ix 0 % _2) <$> (#currentStream <<%= drop 1)
      pure (Just $ Sentence $ maybe (s :| []) (\(x', xs') -> x' :| mconcat [xs', [s]]) (uncons x))

-- we also ignore this DIVIDE_AT_COLON_SYNTAX_CALLBACK nonsense.
-- inform checks if we do not break file boundaries, which we also ignore for now.
-- basically this is a long winded check for 4:50pm (e.g.)
considerColonDivision ::
  Word
  -> Word
  -> Bool
considerColonDivision prev lookA = not $
  matchWord isNumber prev
  && matchWord isNumber lookA
  && (lookA ^. #precedingWhitespace) `elem` [Space, Tab]
