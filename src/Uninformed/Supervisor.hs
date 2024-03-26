module Uninformed.Supervisor where

import Uninformed.Pipeline
{-}
buildProject ::
  ProjectConfig
  -> FilePath
  -> m ()
buildProject conf fp = do
  (sf', _) <- runPipelineStage (Proxy @'LexingStage) sf
  runPipelineStage (Proxy @'SentenceBreakingStage) sf'
  -}