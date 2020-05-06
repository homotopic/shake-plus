module Development.Shake.Plus.FileRules (
  need
, want
, needP
, wantP
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

-- | Lifted version of `Development.Shake.need`, This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use `needP`
need :: MonadAction m => [String] -> m ()
need = liftAction . Development.Shake.need

-- | Lifted version of `Development.Shake.want`. This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use wantP.
want :: MonadRules m => [String] -> m ()
want = liftRules . Development.Shake.want

-- | Lifted version of `Development.Shake.need` using well-typed `Path`s
needP :: (MonadAction m, Partial) => [Path Rel File] -> m ()
needP = need . map toFilePath

-- | Lifted version of `Development.Shake.want` using well-typed `Path`s
wantP :: MonadRules m => [Path Rel File] -> m ()
wantP = want . map toFilePath

-- | Lifted version of `Development.Shake.%>` using well-typed `Path`s
(%>) :: (Partial, MonadReader r m, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(%>) x ract = ask >>= \r -> liftRules $ x Development.Shake.%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.|%>` using well-typed `Path`s
(|%>) :: (Partial, MonadReader r m, MonadRules m) => [FilePattern] -> (Path Rel File -> RAction r ()) -> m ()
(|%>) x ract = ask >>= \r -> liftRules $ x Development.Shake.|%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.phony` using well-typed `Path`s and `RAction`
phony :: (MonadReader r m, MonadRules m) => String -> RAction r () -> m ()
phony x ract = ask >>= \r -> liftRules $ Development.Shake.phony x $ runRAction r ract
