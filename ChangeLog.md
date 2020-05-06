# Changelog for shake-plus

## v0.0.2.1

* Add functions based on [Within](https://hackage.haskell.org/package/path) to better
  keep track of source and target directories.
* Reexport more shake functions to remove need to import vanilla shake qualified.
* Add convenience functions `loadSortFilterApply` and `loadSortFilterApplyW` for batch
  loading via `MonadAction`.
* Add more of the API surface with `Path` and `Within` including variants of `need`, `want`
  `readFile`, `writeFile`. 

## v0.0.1.0

* Initial sketch of shake-plus with reexported functions. Mostly oracles,
  filepaths and directory functions using
  [Path](https://hackage.haskell.org/package/path), and `MonadAction`,
  `MonadUnliftAction` and `MonadRules` with `ReaderT` transformers in a similar
  style to [RIO](https://hackage.haskell.org/package/rio)
