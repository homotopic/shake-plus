module Development.Shake.Plus.Loaders (
  batchLoad
, batchLoadWithin
, batchLoadWithin'
) where

import Development.Shake.Plus.Core
import Development.Shake.Plus.Directory
import RIO
import qualified RIO.HashMap as HM
import Path
import Within
 
traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

-- | Load a directory of `FilePattern`s via some loading function. This should
-- be a `Development.Shake.newCache` operation that takes full filepaths.
batchLoad :: MonadAction m
          => Path b Dir -- ^ The directory to search in
          -> [FilePattern] -- ^ A filepattern to match against.
          -> (Path b File -> m a) -- ^ A `Development.Shake.newCache` operation that loads the file and turns it into some `a`.
          -> m (HashMap (Path Rel File) a)
batchLoad dir pat f = do
  xs <- getDirectoryFiles dir pat >>= mapM (traverseToSnd $ f . fromWithin . (`within` dir))
  return . HM.fromList $ xs

-- | Like `batchLoad`, but returns an `Within` of a `Dir` containing the `HashMap`
batchLoadWithin :: MonadAction m
                => Path b Dir -- ^ The directory to search in
                -> [FilePattern] -- ^ A filepattern to match against.
                -> (Path b File -> m a) -- ^ A `Development.Shake.newCache` operation that loads the file and turns it into some `a`.
                -> m (Within b (HashMap (Path Rel File) a))
batchLoadWithin dir pat f = do
  xs <- getDirectoryFiles dir pat >>= mapM (traverseToSnd $ f . fromWithin . (`within` dir))
  return $ (`within` dir) $ HM.fromList xs

-- | Like `batchLoadWithin'`, but returns a `HashMap` containing `Within` values instead of an `Within` of a `Hashmap`.
batchLoadWithin' :: MonadAction m
                 => Path b Dir -- ^ The directory to search in
                 -> [FilePattern] -- ^ A filepattern to match against.
                 -> (Path b File -> m a) -- ^ A `Development.Shake.newCache` operation that loads the file and turns it into some `a`.
                 -> m (HashMap (Within b (Path Rel File)) a)
batchLoadWithin' dir pat f = do
  xs <- getDirectoryFiles dir pat >>= mapM (traverseToSnd $ f . fromWithin . (`within` dir))
  return $ HM.fromList (first (`within` dir) <$> xs)
