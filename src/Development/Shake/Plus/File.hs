module Development.Shake.Plus.File (
  copyFile
, copyFile'
, copyFileChanged
, copyFileChanged'
, readFile'
, readFileLines
, readFileIn'
, writeFile'
, writeFileLines
, writeFileIn'
, writeFileChanged
, writeFileChangedIn
, removeFiles
, removeFilesAfter
, FileLike(..)
, DirLike(..)
, (/>)
) where

import           Control.Comonad.Env         as E
import           Control.Exception.Extra
import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           Path.Like
import           RIO
import qualified RIO.Text                    as T

-- | Lifted version of `Development.Shake.copyFile` that copies between any two `FileLike`.
copyFile :: (MonadAction m, FileLike b a, FileLike b' a', Partial) => a -> a' -> m ()
copyFile x y = liftAction $ Development.Shake.copyFile' (toFilePath . toFile $ x) (toFilePath . toFile $ y)

-- | Like `copyFile` but for `FileLike`s that are of the same type, useful for type inference.
copyFile' :: (MonadAction m, FileLike b a, Partial) => a -> a -> m ()
copyFile' = copyFile

-- | Lifted version of `Development.Shake.copyFileChanged'` that copies between two `FileLike'.
copyFileChanged :: (MonadAction m, FileLike b a, FileLike b' a', Partial) => a -> a' -> m ()
copyFileChanged x y = liftAction $ Development.Shake.copyFileChanged (toFilePath . toFile $ x) (toFilePath . toFile $ y)

-- | Like `copyFileChanged` but ensures the `FileLike`s are of the same type, useful for type inference.
copyFileChanged' :: (MonadAction m, FileLike b a, Partial) => a -> a -> m ()
copyFileChanged' = copyFileChanged

-- | Lifted version of `Development.Shake.readFile'` that reads any `FileLike`.
readFile' :: (MonadAction m, FileLike b a, Partial) => a -> m Text
readFile' = liftAction . fmap T.pack . Development.Shake.readFile' . toFilePath . toFile

-- | Lifted version of `Development.Shake.readFileLines` that reads any `FileLike`.
readFileLines :: (MonadAction m, FileLike b a, Partial) => a -> m [Text]
readFileLines = liftAction . fmap (fmap T.pack) . Development.Shake.readFileLines . toFilePath . toFile

-- | Like `readFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
readFileIn' :: (MonadAction m, DirLike b d, FileLike Rel a, Partial) => d -> a -> m Text
readFileIn' x y = readFile' $ x /> y

-- | Lifted version of `Development.Shake.writeFile` that writes to any `FileLike`.
writeFile' :: (MonadAction m, FileLike b a, Partial) => a -> Text -> m ()
writeFile' x y = liftAction $ Development.Shake.writeFile' (toFilePath . toFile $ x) (T.unpack y)

-- | Lifted version of `Development.Shake.writeFileLines` that writes to any `FileLike`.
writeFileLines :: (MonadAction m, FileLike b a, Partial) => a -> [Text] -> m ()
writeFileLines x y = liftAction $ Development.Shake.writeFileLines (toFilePath . toFile $ x) (fmap T.unpack y)

-- | Like `writeFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileIn' :: (MonadAction m, DirLike b d, FileLike Rel a, Partial) => d -> a -> Text -> m ()
writeFileIn' x y = writeFile' $ x /> y

-- | Lifted version of `Development.Shake.writeFileChanged` that writes to any `FileLike`.
writeFileChanged :: (MonadAction m, FileLike b a, Partial) => a -> Text -> m ()
writeFileChanged x y = liftAction $ Development.Shake.writeFileChanged (toFilePath . toFile $ x) (T.unpack y)

-- | Like `writeFileChanged'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileChangedIn :: (MonadAction m, DirLike b d, FileLike Rel a, Partial) => d -> a -> Text -> m ()
writeFileChangedIn x y = writeFileChanged $ x /> y

-- | Lifted version of `Development.Shake.removeFiles` that accepts any `DirLike`.
removeFiles :: (MonadAction m, DirLike b d) => d -> [FilePattern] -> m ()
removeFiles x y = liftAction . liftIO $ Development.Shake.removeFiles (toFilePath . toDir $ x) y

-- | Lifted version of `Development.Shake.removeFilesAfter` that accepts any `DirLike`..
removeFilesAfter :: (MonadAction m, DirLike b d) => d -> [FilePattern] -> m ()
removeFilesAfter x y = liftAction $ Development.Shake.removeFilesAfter (toFilePath . toDir $ x) y
