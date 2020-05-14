module Development.Shake.Plus.Loaders (
  batchLoad
, batchLoadWithin
, batchLoadWithin'
) where

import           Control.Comonad.Env as E
import           Development.Shake.Plus.Core
import           Development.Shake.Plus.Directory
import           Path
import           RIO
import qualified RIO.HashMap                      as HM
import           Within

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

-- | Load a directory of `FilePattern`s via some loading function. This should
-- be a `Development.Shake.newCache` operation that takes full filepaths.
batchLoad :: MonadAction m
          => Path b Dir -- ^ The directory to search in
          -> [FilePattern] -- ^ A filepattern to match against.
          -> (Path b File -> m a) -- ^ A `Development.Shake.Plus.newCache` operation that loads the file and turns it into some `a`.
          -> m (HashMap (Path Rel File) a)
batchLoad dir pat f = do
  xs  <- getDirectoryFiles dir pat
  xs' <- mapM (traverseToSnd $ f . (dir </>)) xs
  return . HM.fromList $ xs'

-- | Like `batchLoad`, but returns an `Within` of a `Dir` containing the `HashMap`
batchLoadWithin :: MonadAction m
                => Within b [FilePattern] -- ^ The directory and filepattern to search.
                -> (Within b (Path Rel File) -> m a) -- ^ A `Development.Shake.Plus.newCache` operation that loads the file and turns it into some `a`.
                -> m (Within b (HashMap (Path Rel File) a))
batchLoadWithin w f = do
  xs  <- getDirectoryFiles (E.ask w) (extract w)
  xs' <- mapM (traverseToSnd $ f . (`within` E.ask w)) xs
  return $ (`within` E.ask w) $ HM.fromList xs'

-- | Like `batchLoadWithin'`, but returns a `HashMap` containing `Within` values instead of an `Within` of a `Hashmap`.
batchLoadWithin' :: MonadAction m
                 => Within b [FilePattern] -- ^ The directory and filepattern to search.
                 -> (Within b (Path Rel File) -> m a) -- ^ A `Development.Shake.Plus.newCache` operation that loads the file and turns it into some `a`.
                 -> m (HashMap (Within b (Path Rel File)) a)
batchLoadWithin' w f = do
  xs  <- getDirectoryFiles (E.ask w) (extract w)
  xs' <- mapM (traverseToSnd $ f . (`within` E.ask w)) xs
  return $ HM.fromList (first (`within` E.ask w) <$> xs')
