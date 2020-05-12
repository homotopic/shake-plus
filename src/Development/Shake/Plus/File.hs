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
, asWithin
, within
, fromWithin
) where

import Control.Exception.Extra
import Data.Hashable
import qualified Development.Shake
import Development.Shake.Plus.Core
import RIO
import qualified RIO.Text as T
import Path
import Control.Comonad.Env as E

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

-- | Combine a Comonad env with its extract target
-- implode :: ComonadEnv e w => (e -> a -> c) -> w a -> c
-- implode f = liftA2 f E.ask extract

-- resorb :: Comonad w => (e -> e' -> e'') -> EnvT e (EnvT e' w) a -> EnvT e'' w a
-- resorb f w@(EnvT e (EnvT e' wa)) = EnvT (f (E.ask w) (E.ask $ lower w)) wa

-- h :: MonadAction m => EnvT (Path Rel Dir) (EnvT (Path Rel Dir) (Env (Path Rel File))) Text -> m ()
-- h = implode writeFile' . resorb (</>) . resorb (</>)

-- | Like 'readFile'`, but accepts an `Env` value.
readFileWithin :: MonadAction m => Env (Path Rel Dir) (Path Rel File) -> m Text
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
writeFileWithin :: MonadAction m => Env (Path Rel Dir) (Path Rel File) -> Text -> m ()
writeFileWithin = writeFile' . liftA2 (</>) E.ask extract

-- | Lifted version of `Development.Shake.writeFileChanged` with well-typed `Path`.
writeFileChanged :: (MonadAction m, Partial) => Path b File -> Text -> m ()
writeFileChanged x y = liftAction $ Development.Shake.writeFileChanged (toFilePath x) (T.unpack y)

-- | Like `writeFileChanged'`, but with an argument for the parent directory. Used for symmetry with
-- the way `Development.Shake.getDirectoryFiles` takes arguments.
writeFileChangedIn :: MonadAction m => Path Rel Dir -> Path Rel File -> Text -> m ()
writeFileChangedIn x y = writeFileChanged $ x </> y

-- | Like `writeFileChanged'`, but accepts an `Env` value.
writeFileChangedWithin :: MonadAction m => Env (Path Rel Dir) (Path Rel File) -> Text -> m ()
writeFileChangedWithin = writeFileChanged . liftA2 (</>) E.ask extract

-- | Lifted version of `Development.Shake.removeFiles` with well-typed `Path`.
removeFiles :: MonadAction m => Path b File -> [FilePattern] -> m ()
removeFiles x y = liftAction . liftIO $ Development.Shake.removeFiles (toFilePath x) y

-- | Lifted version of `Development.Shake.removeFilesAfter` with well-typed `Path`.
removeFilesAfter :: MonadAction m => Path Rel Dir -> [FilePattern] -> m ()
removeFilesAfter x y = liftAction $ Development.Shake.removeFilesAfter (toFilePath x) y

-- | Treat a `Path` as if it lies within another directory and returns an Env value.
-- Used infix like
--
-- >>> $(mkRelFile "foo/a.txt") `asWithin` $(mkRelDir "foo")
-- 
asWithin :: MonadThrow m => Path a t -> Path a Dir -> m (Env (Path a Dir) (Path Rel t))
asWithin x y = stripProperPrefix y x >>= \z -> return (EnvT y (Identity z))

-- | Synonym for `flip env`, put a relative path inside a directory.
-- 
-- >>> $(mkRelFile "a.txt") `within` $(mkRelDir "foo")
within :: Path Rel t -> Path a Dir -> Env (Path a Dir) (Path Rel t)
within = flip env

-- | Turns an `Env` directory containing a path into a single path.
fromWithin :: Env (Path a Dir) (Path Rel t) -> Path a t
fromWithin = liftA2 (</>) E.ask extract

instance Eq (Env (Path b Dir) (Path Rel File)) where
  (EnvT e a) == (EnvT e' a') = e == e' && a == a'

instance Hashable (Env (Path b Dir) (Path Rel File)) where
  hashWithSalt n w = hashWithSalt n (fromWithin w)
