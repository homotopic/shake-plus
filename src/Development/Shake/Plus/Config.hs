{- |
   Module     : Development.Shake.Plus.Config
   Copyright  : Copyright (C) 2020 Daniel Firth
   Maintainer : Daniel Firth <dan.firth@homotopic.tech
   License    : MIT
   Stability  : experimental

Utilities in "Development.Shake.Config" lifted to `MonadAction` and `FileLike`/`DirLike`.
-}
module Development.Shake.Plus.Config (
  readConfigFile
, readConfigFileWithEnv
, usingConfigFile
, usingConfig
, getConfig
, getConfigKeys
) where

import           Development.Shake
import qualified Development.Shake.Config
import           Development.Shake.Plus.Core
import           Path
import           Path.Like
import           RIO

-- | Lifted `Development.Shake.Config.readConfigFile` with well-typed path.
readConfigFile :: (MonadIO m, FileLike b a) => a -> m (HashMap String String)
readConfigFile = liftIO . Development.Shake.Config.readConfigFile . toFilePath . toFile

-- | Lifted `Development.Shake.Config.readConfigFileWithEnv` with well-typed path.
readConfigFileWithEnv :: (MonadIO m, FileLike b a) => [(String, String)] -> a -> m (HashMap String String)
readConfigFileWithEnv vars file = liftIO $ Development.Shake.Config.readConfigFileWithEnv vars (toFilePath . toFile $ file)

-- | Lifted `Development.Shake.Config.usingConfigFile` with well-typed path.
usingConfigFile :: (MonadRules m, FileLike b a) => a -> m ()
usingConfigFile = liftRules . Development.Shake.Config.usingConfigFile . toFilePath . toFile

-- | Lifted `Development.Shake.Config.usingConfig`.
usingConfig :: MonadRules m => HashMap String String -> m ()
usingConfig = liftRules . Development.Shake.Config.usingConfig

-- | Lifted `Development.Shake.Config.getConfig`.
getConfig :: MonadAction m => String -> m (Maybe String)
getConfig = liftAction . Development.Shake.Config.getConfig

-- | Lifted `Development.Shake.Config.getConfigKeys`.
getConfigKeys :: MonadAction m => m [String]
getConfigKeys = liftAction Development.Shake.Config.getConfigKeys
