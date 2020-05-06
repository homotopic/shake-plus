module Development.Shake.Plus.Loaders (
  loadSortFilterApply
, loadSortFilterApplyW
) where

import Development.Shake.Plus.Core
import Development.Shake.Plus.Directory
import RIO
import RIO.List
import Path
import Within

traverseToSnd :: Functor f => (a -> f b) -> a -> f (a, b)
traverseToSnd f a = (a,) <$> f a

-- | Batch loading function. Loads all items detected by the filtepattern and provides parameters
-- for sorting, filtering and a simple transformation via an endofunction. Returns a list of values
-- indexed by their source `Path`.
loadSortFilterApply :: (MonadAction m, Ord b)
                    => (Path Rel File -> m a) -- ^ A loading function, (i.e readFile' or a custom `readMarkdownFile, readJPG etc that returns a type you can sort/filter on.)
                    -> Path Rel Dir -- ^ The path to search in.
                    -> [FilePattern] -- ^ A filepattern to match on.
                    -> (a -> b) -- ^ The value to sortOn .
                    -> (a -> Bool) -- ^ A filtering predicate.
                    -> (a -> a) -- ^ A simple endotransformation.
                    -> m [(Path Rel File, a)]
loadSortFilterApply l dir pat s f e = do
  xs <- getDirectoryFiles dir pat >>= mapM (traverseToSnd l)
  return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) $ xs

-- | Like `loadSortFilterApply`, but returns `Within` values.
loadSortFilterApplyW :: (MonadAction m, Ord b)
                     => (Within Rel File -> m a) -- ^ A loading function, (i.e readFile' or a custom `readMarkdownFile, readJPG etc that returns a type you can sort/filter on.)
                     -> Path Rel Dir -- ^ The path to search in.
                     -> [FilePattern] -- ^ A filepattern to match on.
                     -> (a -> b) -- ^ The value to sortOn .
                     -> (a -> Bool) -- ^ A filtering predicate.
                     -> (a -> a) -- ^ A simple endotransformation.
                     -> m [(Within Rel File, a)]
loadSortFilterApplyW l dir pat s f e = do
  xs <- getDirectoryFilesWithin dir pat >>= mapM (traverseToSnd l)
  return $ fmap (second e) $ sortOn (s . snd) $ filter (f . snd) $ xs
