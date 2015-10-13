first-and-last
==============

[![first-and-last on Hackage](https://img.shields.io/hackage/v/first-and-last.svg)](https://hackage.haskell.org/package/first-and-last) [![first-and-last on Travis CI](https://travis-ci.org/markandrus/first-and-last.svg)](https://travis-ci.org/markandrus/first-and-last)

This library provides data types [`First' n`](https://markandrus.github.io/first-and-last/Data-Monoid-First.html#t:First-39-) and [`Last' n`](https://markandrus.github.io/first-and-last/Data-Monoid-Last.html#t:Last-39-) generalizing `First` and `Last` from [`Data.Monoid`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html) to return up to `n` values.

```hs
getFirst' (foldMap pure [1, 2, 3, 4] :: First' 2 Int) ≡ [1, 2]
getLast'  (foldMap pure [1, 2, 3, 4] :: Last'  2 Int) ≡ [3, 4]
```

It also provides API-compatible type synonyms [`First`](https://markandrus.github.io/first-and-last/Data-Monoid-First.html#t:First) and [`Last`](https://markandrus.github.io/first-and-last/Data-Monoid-Last.html#t:Last) as well as functions [`getFirst`](https://markandrus.github.io/first-and-last/Data-Monoid-First.html#v:getFirst) and [`getLast`](https://markandrus.github.io/first-and-last/Data-Monoid-Last.html#v:getLast), allowing you to use it as a drop-in replacement.

Documentation available on [Hackage](#). For HEAD, see [markandrus.github.io/first-and-last](https://markandrus.github.io/first-and-last).

Install
-------

Install using

```
$ cabal install first-and-last
```

Contributing
------------

Feel free to contribute to any of the open [issues]
(https://github.com/markandrus/first-and-last/issues), bugfixes, etc. When you
think you're ready to merge, ensure the tests are passing and open a pull
request. If you are adding new functionality, please include new tests as well.
Finally, add yourself to the `AUTHORS` file.
