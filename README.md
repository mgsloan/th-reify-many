# `th-reify-many`
[![Hackage](https://img.shields.io/hackage/v/th-reify-many.svg)][Hackage: th-reify-many]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-reify-many.svg)](http://packdeps.haskellers.com/reverse/th-reify-many)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/mgsloan/th-reify-many/workflows/Haskell-CI/badge.svg)](https://github.com/mgsloan/th-reify-many/actions?query=workflow%3AHaskell-CI)

[Hackage: th-reify-many]:
  http://hackage.haskell.org/package/th-reify-many
  "th-reify-many package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`th-reify-many` provides functions for recursively reifying top
level declarations.  The main intended use case is for enumerating
the names of datatypes reachable from an initial datatype, and
passing these names to some function which generates instances.
