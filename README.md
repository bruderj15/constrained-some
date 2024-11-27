[![Hackage](https://img.shields.io/hackage/v/constrained-some.svg)](https://hackage.haskell.org/package/constrained-some) ![Static Badge](https://img.shields.io/badge/Lang-Haskell2010-blue) [![Haskell-CI](https://github.com/bruderj15/constrained-some/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/Hasmtlib/actions/workflows/haskell-ci.yml) [![License: GPL v3](https://img.shields.io/badge/License-MIT-blue.svg)](https://mit-license.org/)

# constrained-some

This library provides utilities for working with existential types and type-level constraints.
It allows you to enforce multiple constraints on polymorphic types and containers complementing `some`.

## Core

- **Existential Types**: `Somes` and `Somes1` provide existential wrappers for types with multiple constraints.
- **Convenient Aliases**: Simplified types `Some` and `Some1` for scenarios where just one constraint is needed.

## Usage

```haskell
import Data.Some.Constraint

someShowableOrd :: Somes '[Show, Ord]
someShowableOrd = Some (mempty :: [Double])

someNumFunctor :: Some1 Functor Num
someNumFunctor = Some1 $ [1, 2, 3 :: Int]
```

## Contact information
Contributions, critics and bug reports are welcome!

Please feel free to contact me through GitHub.
