module Development.Shake.Plus.Directory (
  doesFileExist
, doesDirectoryExist
, getDirectoryFiles
, getDirectoryFilesWithin
, getDirectoryDirs
, getDirectoryFilesIO
) where

import Control.Comonad.Env
import Development.Shake.Plus.Core
import qualified Development.Shake
import RIO
import Path

-- | Lifted version of `Development.Shake.doesFileExist` using well-typed `Path`s.
doesFileExist :: MonadAction m => Path b File -> m Bool
doesFileExist = liftAction . Development.Shake.doesFileExist . toFilePath

-- | Lifted version of `Development.Shake.doesDirectoryExist` using well-typed `Path`s.
doesDirectoryExist :: MonadAction m => Path b Dir -> m Bool
doesDirectoryExist = liftAction . Development.Shake.doesDirectoryExist . toFilePath

-- | Lifted version of `Development.Shake.getDirectoryFiles` using well-typed `Path`s.
getDirectoryFiles :: MonadAction m => Path b Dir -> [FilePattern] -> m [Path Rel File]
getDirectoryFiles x y = liftAction $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFiles (toFilePath x) y

-- | Like `getDirectoryFiles`, but returns `Within` values.
getDirectoryFilesWithin :: MonadAction m => Path Rel Dir -> [FilePattern] -> m (Env (Path Rel Dir) [(Path Rel File)])
getDirectoryFilesWithin x pat = do
  xs <- getDirectoryFiles x pat
  return (env x xs)

-- | Lifted version of `Development.Shake.getDirectoryDirs` using well-typed `Path`s.
getDirectoryDirs :: MonadAction m => Path b Dir -> m [Path Rel Dir]
getDirectoryDirs x = liftAction $ traverse (liftIO . parseRelDir) =<< Development.Shake.getDirectoryDirs (toFilePath x)

-- | Lifted version of `Development.Shake.getDirectoryFilesIO` using well-typed `Path`s.
getDirectoryFilesIO :: MonadIO m => Path b Dir -> [FilePattern] -> m [Path Rel File]
getDirectoryFilesIO x y = liftIO $ traverse (liftIO . parseRelFile) =<< Development.Shake.getDirectoryFilesIO (toFilePath x) y
