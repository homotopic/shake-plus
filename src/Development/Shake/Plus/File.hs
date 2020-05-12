module Development.Shake.Plus.File (
  copyFile'
, copyFileChanged
, readFile'
, readFileLines
, readFileIn'
, readFileWithin
, writeFile'
, writeFileLines
, writeFileIn'
, writeFileWithin
, writeFileChanged
, writeFileChangedIn
, writeFileChangedWithin
, removeFiles
, removeFilesAfter
) where

import Control.Exception.Extra
import qualified Development.Shake
import Development.Shake.Plus.Core
import RIO
import qualified RIO.Text as T
import Path
import Control.Comonad.Env as E
import Within

-- | Lifted version of `Development.Shake.copyFile` with well-typed `Path`s.
copyFile' :: (MonadAction m, Partial) => Path Rel File -> Path Rel File -> m ()
copyFile' x y = liftAction $ Development.Shake.copyFile' (toFilePath x) (toFilePath y)

-- | Lifted version of `Development.Shake.copyFileChanged'` with well-typed `Path`s.
copyFileChanged :: (MonadAction m, Partial) => Path Rel File -> Path Rel File -> m ()
copyFileChanged x y = liftAction $ Development.Shake.copyFileChanged (toFilePath x) (toFilePath y)

-- | Lifted version of `Development.Shake.readFile'` with well-typed `Path`.
readFile' :: (MonadAction m, Partial) => Path Rel File -> m Text
readFile' = liftAction . fmap T.pack . Development.Shake.readFile' . toFilePath

-- | Lifted version of `Development.Shake.readFileLines` with well-typed `Path`.
readFileLines :: (MonadAction m, Partial) => Path Rel File -> m [Text]
readFileLines = liftAction . fmap (fmap T.pack) . Development.Shake.readFileLines . toFilePath

-- | Like `readFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
readFileIn' :: MonadAction m => Path Rel Dir -> Path Rel File -> m Text
readFileIn' x y = readFile' $ x </> y

-- | Like 'readFile'`, but accepts an `Env` value.
readFileWithin :: MonadAction m => Within Rel (Path Rel File) -> m Text
readFileWithin = readFile' . liftA2 (</>) E.ask extract

-- | Lifted version of `Development.Shake.writeFile` with well-typed `Path`.
writeFile' :: (MonadAction m, Partial) => Path Rel File -> Text -> m ()
writeFile' x y = liftAction $ Development.Shake.writeFile' (toFilePath x) (T.unpack y)

-- | Lifted version of `Development.Shake.writeFileLines` with well-typed `Path`.
writeFileLines :: (MonadAction m, Partial) => Path Rel File -> [Text] -> m ()
writeFileLines x y = liftAction $ Development.Shake.writeFileLines (toFilePath x) (fmap T.unpack y)

-- | Like `writeFile'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileIn' :: MonadAction m => Path Rel Dir -> Path Rel File -> Text -> m ()
writeFileIn' x y = writeFile' $ x </> y

-- | Like 'writeFile'`, but accepts a `Env` value.
writeFileWithin :: MonadAction m => Within Rel (Path Rel File) -> Text -> m ()
writeFileWithin = writeFile' . liftA2 (</>) E.ask extract

-- | Lifted version of `Development.Shake.writeFileChanged` with well-typed `Path`.
writeFileChanged :: (MonadAction m, Partial) => Path b File -> Text -> m ()
writeFileChanged x y = liftAction $ Development.Shake.writeFileChanged (toFilePath x) (T.unpack y)

-- | Like `writeFileChanged'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileChangedIn :: MonadAction m => Path Rel Dir -> Path Rel File -> Text -> m ()
writeFileChangedIn x y = writeFileChanged $ x </> y

-- | Like `writeFileChanged'`, but accepts an `Env` value.
writeFileChangedWithin :: MonadAction m => Within Rel (Path Rel File) -> Text -> m ()
writeFileChangedWithin = writeFileChanged . liftA2 (</>) E.ask extract

-- | Lifted version of `Development.Shake.removeFiles` with well-typed `Path`.
removeFiles :: MonadAction m => Path b File -> [FilePattern] -> m ()
removeFiles x y = liftAction . liftIO $ Development.Shake.removeFiles (toFilePath x) y

-- | Lifted version of `Development.Shake.removeFilesAfter` with well-typed `Path`.
removeFilesAfter :: MonadAction m => Path Rel Dir -> [FilePattern] -> m ()
removeFilesAfter x y = liftAction $ Development.Shake.removeFilesAfter (toFilePath x) y
