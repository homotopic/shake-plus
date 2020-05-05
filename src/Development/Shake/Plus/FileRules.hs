module Development.Shake.Plus.FileRules (
  need
, (%>)
, (|%>)
, phony
) where

import Control.Exception.Extra
import Development.Shake.Plus.Core
import qualified Development.Shake
import Development.Shake (FilePattern)
import RIO
import Path

-- | Lifted version of `Development.Shake.need` using well-typed `Path`s
need :: (MonadAction m, Partial) => [Path Rel File] -> m ()
need = liftAction . Development.Shake.need . map toFilePath

-- | Lifted version of `Development.Shake.%>` using well-typed `Path`s
(%>) :: (Partial, MonadReader r m, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(%>) x ract = ask >>= \r -> liftRules $ x Development.Shake.%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.|%>` using well-typed `Path`s
(|%>) :: (Partial, MonadReader r m, MonadRules m) => [FilePattern] -> (Path Rel File -> RAction r ()) -> m ()
(|%>) x ract = ask >>= \r -> liftRules $ x Development.Shake.|%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.phony` using well-typed `Path`s
phony :: (MonadReader r m, MonadRules m) => String -> RAction r () -> m ()
phony x ract = ask >>= \r -> liftRules $ Development.Shake.phony x $ runRAction r ract
