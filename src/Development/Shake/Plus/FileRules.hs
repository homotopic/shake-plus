{- |
   Module     : Development.Shake.Plus.FileRules
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Filerules in "Development.Shake" lifted to `MonadAction` and `FileLike`/`DirLike`.
-}
module Development.Shake.Plus.FileRules (
  need
, want
, needP
, wantP
, needIn
, wantIn
, needWithin
, wantWithin
, (%>)
, (|%>)
, (%^>)
, (|%^>)
, phony
) where

import           Control.Comonad.Env         as E
import           Control.Exception.Extra
import qualified Development.Shake
import qualified Development.Shake.FilePath
import           Development.Shake.Plus.Core
import           Path
import           Path.Like
import           RIO                         as R
import           Within

-- | Lifted version of `Development.Shake.need`, This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use `needP`
need :: (Partial, MonadAction m, Foldable t) => t String -> m ()
need = liftAction . Development.Shake.need . toList

-- | Lifted version of `Development.Shake.want`. This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use wantP.
want :: (Partial, MonadRules m, Foldable t) => t String -> m ()
want = liftRules . Development.Shake.want . toList

-- | Lifted version of `Development.Shake.need` using well-typed `Path`s
needP :: (Partial, MonadAction m, Traversable t, FileLike b a) => t a -> m ()
needP = need . fmap (toFilePath . toFile)

-- | Lifted version of `Development.Shake.want` using well-typed `Path`s
wantP :: (Partial, MonadRules m, Traversable t, FileLike b a) => t a -> m ()
wantP = want . fmap (toFilePath . toFile)

-- | Like `needP`, but accepts `Path`s relative to the first argument.
needIn :: (Partial, MonadAction m, Traversable t, DirLike Rel d, FileLike Rel a) => d -> t a -> m ()
needIn x = needP . fmap (x />)

-- | Like `wantP`, but accepts `Path`s relative to the first argument.
wantIn :: (Partial, MonadRules m, Traversable t, DirLike Rel d, FileLike Rel a) => d -> t a -> m ()
wantIn x = wantP . fmap (x />)

-- | Like `needIn`, but accepts a list of `Path`s inside a `Within` value.
needWithin :: (Partial, MonadAction m, Traversable t, FileLike Rel a) => Within Rel (t a) -> m ()
needWithin x = needIn (E.ask x) (extract x)

-- | Like `wantIn`, but accepts a list of `Path`s insides a `Within` value.
wantWithin :: (Partial, MonadRules m, Traversable t, FileLike Rel a) => Within Rel (t a) -> m ()
wantWithin x = wantIn (E.ask x) (extract x)

-- | Lifted version of `Development.Shake.%>` using well-typed `Path`s
(%>) :: (Partial, MonadReader r m, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(%>) x ract = R.ask >>= \r -> liftRules $ x Development.Shake.%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.|%>` using well-typed `Path`s
(|%>) :: (Partial, MonadReader r m, MonadRules m) => [FilePattern] -> (Path Rel File -> RAction r ()) -> m ()
(|%>) x ract = R.ask >>= \r -> liftRules $ x Development.Shake.|%> (runRAction r . (ract <=< parseRelFile))

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel FilePattern -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(%^>) xs ract = liftA2 (Development.Shake.FilePath.</>) (toFilePath . E.ask) extract xs %> (ract <=< (`asWithin` E.ask xs))

-- | `Within` variant of `(%>)`, used to keep track of local directories.
(|%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel [FilePattern] -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
(|%^>) xs ract = ((Development.Shake.FilePath.</>) (toFilePath . E.ask $ xs) <$> extract xs) |%> (ract <=< (`asWithin` E.ask xs))

-- | Lifted version of `Development.Shake.phony` using `RAction`
phony :: (MonadReader r m, MonadRules m) => String -> RAction r () -> m ()
phony x ract = R.ask >>= \r -> liftRules $ Development.Shake.phony x $ runRAction r ract
