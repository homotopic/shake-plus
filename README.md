# Shake+ - Super Powered Shake

Attempt at a batteries included Shake. We reexport replacements for the main
utility functions of Shake with the following adjustments whereever possible.

* Well-typed paths using the [path](https://hackage.haskell.org/package/path)
* New type classes `MonadAction`,`MonadUnliftAction` and `MonadRules` with
  stock `ReaderT` transformers.
* `Text` instead of `String` wherever it is appropriate.

Unlifting `Action`s is a challenge when we cross monad boundaries (from
`Action` to `Rules`), and so some functions are hard coded against the
`RAction` type, which is a hardcoded newtype `ReaderT r Action a`. This is
annoying, but it's sufficient to add your own logging and reader lenses.  It
may be possible to generalize this with some unlifting contortion but so far I
haven't been able to figure it out.

The main entry point ot this library is the `runShakePlus` function, which
collapses a `ReaderT r Rules ()` to a `Rules ()` and passes the environment to
each underlying `RAction`.

This is an early release and a lot of things may be missing or broken, but so
far the conveniences have been worth it. Until it's stable/complete you probaly
want to `import qualified Development.Shake` to deal with any missing parts.
