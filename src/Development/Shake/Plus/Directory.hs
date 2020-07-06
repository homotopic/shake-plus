module Development.Shake.Plus.Directory (
  doesFileExist
, doesDirectoryExist
, getDirectoryFiles
, getDirectoryFilesWithin
, getDirectoryFilesWithin'
, getDirectoryDirs
, getDirectoryFilesIO
, getDirectoryFilesWithinIO
, getDirectoryFilesWithinIO'
) where

import           Control.Comonad.Env as E
import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           Path.Like
import           RIO
import           Within

-- | Lifted version of `Development.Shake.doesFileExist` using well-typed `Path`s.
doesFileExist :: (MonadAction m, FileLike b a) => a -> m Bool
doesFileExist = liftAction . Development.Shake.doesFileExist . toFilePath . toFile

-- | Lifted version of `Development.Shake.doesDirectoryExist` using well-typed `Path`s.
doesDirectoryExist :: (MonadAction m, DirLike b a) => a -> m Bool
doesDirectoryExist = liftAction . Development.Shake.doesDirectoryExist . toFilePath . toDir

-- | Lifted version of `Development.Shake.getDirectoryFiles` using well-typed `Path`s.
getDirectoryFiles :: (MonadAction m, DirLike b a) => a -> [FilePattern] -> m [Path Rel File]
getDirectoryFiles x y = liftAction $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFiles (toFilePath . toDir $ x) y

-- | Like `getDirectoryFiles`, but accepts a `Within` value and returns a `Within` contaning a list of `Path`s
getDirectoryFilesWithin :: MonadAction m => Within b [FilePattern] -> m (Within b [Path Rel File])
getDirectoryFilesWithin x = do
  xs <- getDirectoryFiles (E.ask x) (extract x)
  return (xs <$ x)

-- | Like `getDirectoryFilesWithin`, but returns a list of `Within` values instead of a `Within`` of a list.
getDirectoryFilesWithin' :: MonadAction m => Within b [FilePattern] -> m [Within b (Path Rel File)]
getDirectoryFilesWithin' x = do
  xs <- getDirectoryFiles (E.ask x) (extract x)
  return ((<$ x) <$> xs)

-- | Lifted version of `Development.Shake.getDirectoryDirs` using well-typed `Path`s.
getDirectoryDirs :: (MonadAction m, DirLike b a) => a -> m [Path Rel Dir]
getDirectoryDirs x = liftAction $ traverse (liftIO . parseRelDir) =<< Development.Shake.getDirectoryDirs (toFilePath . toDir $ x)

-- | Lifted version of `Development.Shake.getDirectoryFilesIO` using well-typed `Path`s.
getDirectoryFilesIO :: (MonadIO m, DirLike b a) => a -> [FilePattern] -> m [Path Rel File]
getDirectoryFilesIO x y = liftIO $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFilesIO (toFilePath . toDir $ x) y

-- | Like `getDirectoryFilesIO`, but accepts a `Within` value and returns a `Within` contaning a list of `Path`s
getDirectoryFilesWithinIO :: MonadIO m => Within b [FilePattern] -> m (Within b [Path Rel File])
getDirectoryFilesWithinIO x = do
  xs <- getDirectoryFilesIO (E.ask x) (extract x)
  return (xs <$ x)

-- | Like `getDirectoryFilesWithinIO`, but returns a list of `Within` values instead of a `Within`` of a list.
getDirectoryFilesWithinIO' :: MonadIO m => Within b [FilePattern] -> m [Within b (Path Rel File)]
getDirectoryFilesWithinIO' x = do
  xs <- getDirectoryFilesIO (E.ask x) (extract x)
  return ((<$ x) <$> xs)
