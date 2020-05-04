module Development.Shake.Plus.Config (
  readConfigFile
, readConfigFileWithEnv
, usingConfigFile
, Development.Shake.Config.usingConfig
, Development.Shake.Config.getConfig
, Development.Shake.Config.getConfigKeys
) where

import Development.Shake
import qualified Development.Shake.Config
import Path
import RIO
import qualified RIO.HashMap as HM

-- | Lifted `Development.Shake.Config.readConfigFile` with well-typed path.
readConfigFile :: MonadIO m => Path a File -> m (HM.HashMap String String)
readConfigFile = liftIO . Development.Shake.Config.readConfigFile . toFilePath

-- | Lifted `Development.Shake.Config.readConfigFileWithEnv` with well-typed path.
readConfigFileWithEnv :: MonadIO m => [(String, String)] -> Path a File -> m (HM.HashMap String String)
readConfigFileWithEnv vars file = liftIO $ Development.Shake.Config.readConfigFileWithEnv vars (toFilePath file)

-- | Lifted `Development.Shake.Config.usingConfigFile` with well-typed path.
usingConfigFile :: Path a File -> Rules ()
usingConfigFile = Development.Shake.Config.usingConfigFile . toFilePath
