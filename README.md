# concat-everywhere

A very work-in-progress attempt at adding function concatenation to Haskell.

```haskell
ghci> -- 1 dup succ + .
ghci> ((stack ~< 1 `dup`) ~ succ ~~ (+) `pop`) :: Int
3
```

> A brief note on a different `Control.Concatenative` from [concatenative](https://hackage.haskell.org/package/concatenative):
> This version faithfully reproduces a number of [Factor](https://factorcode.org)
> combinators, functions, and operators, but as far as I'm able to tell, as of
> version 1.0.1 it still performs function _application_ rather than function
> _concatenation_. It's not a small task to encode these patterns in Haskell's
> type system (or any non-concatenative type system) but function _concatenation_
> is what `concat-everywhere` is more interested in.
