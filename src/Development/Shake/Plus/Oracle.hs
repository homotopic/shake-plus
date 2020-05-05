module Development.Shake.Plus.Oracle (
  addOracle
, addOracleCache
, addOracleHash
, askOracle
, askOracles
) where

import Control.Exception.Extra
import qualified Development.Shake
import Development.Shake (RuleResult, ShakeValue)
import Development.Shake.Plus.Core
import RIO

addOracle :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
          => (q -> RAction r a)
          -> m (q -> RAction r a)
addOracle ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracle (runRAction r . ract)

addOracleCache :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
               => (q -> RAction r a)
               -> m (q -> RAction r a)
addOracleCache ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracleCache (runRAction r . ract)

addOracleHash :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
               => (q -> RAction r a)
               -> m (q -> RAction r a)
addOracleHash ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracleHash (runRAction r . ract)

askOracle :: (MonadAction m, RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> m a
askOracle = liftAction . Development.Shake.askOracle

askOracles :: (MonadAction m, RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q]-> m [a]
askOracles = liftAction . Development.Shake.askOracles
 
