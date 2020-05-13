module Development.Shake.Plus.FileRules (
  need
, want
, needP
, wantP
, needIn
, wantIn
, needWithin
, wantWithin
, needWithin'
, wantWithin'
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
needP :: (Partial, MonadAction m, Traversable t) => t (Path b File) -> m ()
needP = need . fmap toFilePath

-- | Lifted version of `Development.Shake.want` using well-typed `Path`s
wantP :: (Partial, MonadRules m, Traversable t) => t (Path b File) -> m ()
wantP = want . fmap toFilePath

-- | Like `needP`, but accepts `Path`s relative to the first argument.
needIn :: (Partial, MonadAction m, Traversable t) => Path b Dir -> t (Path Rel File) -> m ()
needIn x = needP . fmap (x </>)

-- | Like `wantP`, but accepts `Path`s relative to the first argument.
wantIn :: (Partial, MonadRules m, Traversable t) => Path b Dir -> t (Path Rel File) -> m ()
wantIn x = wantP . fmap (x </>)

-- | Like `needIn`, but accepts a list of `Path`s inside a `Within` value.
needWithin :: (Partial, MonadAction m, Traversable t) => Within b (t (Path Rel File)) -> m ()
needWithin x = needIn (E.ask x) (extract x)

-- | Like `wantIn`, but accepts a list of `Path`s insides a `Within` value.
wantWithin :: (Partial, MonadRules m, Traversable t) => Within b (t (Path Rel File)) -> m ()
wantWithin x = wantIn (E.ask x) (extract x)

-- | Like `needWithin`, but accepts a list of `Within`s instead of a `Within` of a list.
needWithin' :: (Partial, MonadAction m, Traversable t) => t (Within b (Path Rel File)) -> m ()
needWithin' x = needP $ fromWithin <$> x

-- | Like `wantWithin`, but accepts a list of `Within`s instead of a `Within` of a list.
wantWithin' :: (Partial, MonadRules m, Traversable t) => t (Within b (Path Rel File)) -> m ()
wantWithin' x = wantP $ fromWithin <$> x

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
