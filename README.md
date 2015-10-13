first-and-last
==============

[![first-and-last on Hackage](https://img.shields.io/hackage/v/first-and-last.svg)](https://hackage.haskell.org/package/first-and-last) [![first-and-last on Travis CI](https://travis-ci.org/markandrus/first-and-last.svg)](https://travis-ci.org/markandrus/first-and-last)

This library provides data types `First' n` and `Last' n` generalizing `First` and `Last` from [`Data.Monoid`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html) to return up to `n` values.

```
Data.Monoid.First    a ≡
            First' 1 a ≡
            First    a
```

```
Data.Monoid.Last    a ≡
            Last' 1 a ≡
            Last    a
```

It also provides API-compatible type synonyms `First` and `Last` as well as functions `getFirst` and `getLast`, allowing you to use it as a drop-in replacement.

Install
-------

Install using

```
$ cabal install first-and-left
```

Documentation soon to be available on Hackage. For now, see [markandrus.github.io/first-and-last](http://markandrus.github.io/first-and-last).

Contributing
------------

Feel free to contribute to any of the open [issues]
(https://github.com/markandrus/first-and-last/issues), bugfixes, etc. When you
think you're ready to merge, ensure the tests are passing and open a pull
request. If you are adding new functionality, please include new tests as well.
Finally, add yourself to the `AUTHORS` file.
