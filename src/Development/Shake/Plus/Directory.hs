module Development.Shake.Plus.Directory (
  doesFileExist
, doesDirectoryExist
, getDirectoryFiles
, getDirectoryFilesWithin
, getDirectoryFilesWithin'
, getDirectoryDirs
, getDirectoryFilesIO
) where

import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           RIO
import           Within

-- | Lifted version of `Development.Shake.doesFileExist` using well-typed `Path`s.
doesFileExist :: MonadAction m => Path b File -> m Bool
doesFileExist = liftAction . Development.Shake.doesFileExist . toFilePath

-- | Lifted version of `Development.Shake.doesDirectoryExist` using well-typed `Path`s.
doesDirectoryExist :: MonadAction m => Path b Dir -> m Bool
doesDirectoryExist = liftAction . Development.Shake.doesDirectoryExist . toFilePath

-- | Lifted version of `Development.Shake.getDirectoryFiles` using well-typed `Path`s.
getDirectoryFiles :: MonadAction m => Path b Dir -> [FilePattern] -> m [Path Rel File]
getDirectoryFiles x y = liftAction $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFiles (toFilePath x) y

-- | Like `getDirectoryFiles`, but returns a `Within` contaning a list of `Path`s.
getDirectoryFilesWithin :: MonadAction m => Path b Dir -> [FilePattern] -> m (Within b [Path Rel File])
getDirectoryFilesWithin x pat = do
  xs <- getDirectoryFiles x pat
  return (xs `within` x)

-- | Like `getDirectoryFilesWithin`, but returns a list of `Within` values instead of a `Within`` of a list.
getDirectoryFilesWithin' :: MonadAction m => Path b Dir -> [FilePattern] -> m [Within b (Path Rel File)]
getDirectoryFilesWithin' x pat = do
  xs <- getDirectoryFiles x pat
  return ((`within` x) <$> xs)

-- | Lifted version of `Development.Shake.getDirectoryDirs` using well-typed `Path`s.
getDirectoryDirs :: MonadAction m => Path b Dir -> m [Path Rel Dir]
getDirectoryDirs x = liftAction $ traverse (liftIO . parseRelDir) =<< Development.Shake.getDirectoryDirs (toFilePath x)

-- | Lifted version of `Development.Shake.getDirectoryFilesIO` using well-typed `Path`s.
getDirectoryFilesIO :: MonadIO m => Path b Dir -> [FilePattern] -> m [Path Rel File]
getDirectoryFilesIO x y = liftIO $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFilesIO (toFilePath x) y
