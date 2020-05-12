# Shake+ - Super Powered Shake

Attempt at a batteries included Shake. We reexport replacements for the main
utility functions of Shake with the following adjustments whereever possible.

* Well-typed paths using the [path](https://hackage.haskell.org/package/path)
* New type classes `MonadAction`, `MonadUnliftAction` and `MonadRules` with
  concrete `ReaderT` transformers:
  * `RAction r a = RAction (ReaderT r Action a)` and
  * `ShakePlus r a = ShakePlus (ReaderT r Rules a)`
* `Text` instead of `String` wherever it is appropriate.
* `within` style variants of the standard file and directory operations that
  in some cases return or accept `Within b (Path Rel File)` values to keep tags
of parent directories.

This is an early release and some things may be missing or broken, but so
far the conveniences have been worth it. Some notes on the approach are
detailed below.

## Paths

Using the [path](https://hackage.haskell.org/package/path) library is kind of a
no brainer. I lose a lot of time to problems that could be avoided by using
this library, so it's everywhere. The names for these functions shadow the
existing names, so you may want to `import qualified Development.Shake` while
this library progresses if you have other `FilePath` based Shake rules that
you want to mix into your build.

The standard `Development.Shake.FilePath` functions for directory manipulation
are not re-exported in full, and you should use the functions in the path
library (such as `replaceExtension`) and other path-based libraries. This will
probably change.

`FilePattern`s are kept as-is, as `Path` is strongly normalizing it makes
sense to keep these as `Strings`.

## RAction

The `ReaderT r Action a` transformer (called `RAction`) is similar to the
[RIO](https://hackage.haskell.org/package/rio) type and should be used
similarly. In fact, you can reuse the logging functions from `RIO` within any
`RAction` block, which is one of the main motivators for having an `Action`
which is also a `MonadReader`. If you need to reuse an existing shake
`Action` in an `RAction`, use `liftAction`.

## Using Within

One common complaint about Shake is having to keep track of source and output
directories and translating `FilePath`s when using the input to an `Action`,
leading to lots of repetition of the form `(sourceFolder </>) . (-<.> ".ext") .
dropDirectory1` which is prone to breakage. Using `Path` helps this to some
degree, but in some cases is even more annoying because lots of `Path`
functions use `MonadThrow`, leading to lots of monadic steps inside an
`RAction`.

To alleviate this somewhat, we use `Within b (Path Rel File)` as a standard
pattern for representing a file within a directory. `Within` is a type
available in the [within](https://hackage.haskell.org/package/within) package
that is simply a newtype wrapper over an `Env` comonad with the environment
specialized to `Path b Dir`. We provide variants of the file operations and
rules that typically accept or return `Path`s or contain callbacks that expect
paths and change these to `Within` values. These functions are generally
suffixed `within`. Here is the variant of `getDirectoryFiles` that
produces `Within` values.

```{.haskell}
getDirectoryFilesWithin' :: MonadAction m => Path b Dir -> [FilePattern] -> m [Within b (Path Rel File)]
```

You can convert to and from this within-style using `within` and `fromWithin`.

```{.haskell}
let x = $(mkRelFile "a.txt") `within` $(mkRelDir "foo") -- Within Rel (Path Rel File)
fromWithin x -- produces a `Path Rel File`
```

and you can assert that an existing path lies in a directory by using `asWithin`, which throws
if the directory is not a proper prefix of the `Path`.

```{.haskell}
$(mkRelFile "foo/a.txt") `asWithin` $(mkRelDir "foo") -- fine
$(mkRelFile "a.txt") `asWithin` $(mkRelDir "foo") -- throws error
```

Filerules such as `(%>)` have within-style variants that accept an ` (Path b
Dir) FilePattern` on the left and carry that env to the callback.

```{.haskell}
(%^>) :: (Partial, MonadReader r m, MonadRules m) => Within Rel FilePattern -> (Within Rel (Path Rel File) -> RAction r ()) -> m ()
```

You change the underlying filepath with `fmap` or `mapM`, whilst you can move
to a new parent directory by using `localDir, or `localDirM` which is defined
in the `Within` library for when the map between parent directories may throw.
The `Within` library also contains more functions and instances for more
precise changes between output and source directories.

## runShakePlus

The main entry point to this library is the `runShakePlus` function, which
collapses a `ReaderT r Rules ()` to a `Rules ()` and passes the environment to
each underlying `RAction`. The `r`s in `ShakePlus` and the underlying
`RAction`s have to match. A typical setup might look like this.

```{.haskell}
let r = --setup env here
shake shakeOptions $ do

    -- include some regular shake rules.

    runShakePlus r $ do

      -- some shake-plus rules.
```
