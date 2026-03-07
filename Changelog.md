# 0.7.4

- Export new function `gshowsF` that allows more flexibility than `gshows`.
  It allows users to override how some children types should be printed. (https://github.com/dreixel/syb/pull/55)

# 0.7.3

- Fix `gread` to recognize negative numbers (https://github.com/dreixel/syb/issues/13)
- Bump minimum required GHC to 8.0
- `Generic'` is now a newtype instead of data, add `GenericR'` and `GenericB'` (https://github.com/dreixel/syb/issues/49)

# 0.7.2.4
- Improved documentation (thanks to @BinderDavid)
- Export `ext2` function which was already defined but not exported

# 0.7.2.3
- Compatibility with `mtl` 2.3 and GHC 9.6

# 0.7.2.2
- Compatibility with GHC 9.4

# 0.7.2.1
- Update cabal version

# 0.7.2
- Add compatibility with GHC 9, switch to tasty for tests, fix tests on GHCJS

# 0.7.1
- Define recursive traversals in two parts, non-recursive wrapper and recursive local helper to facilitate inlining and avoid passing the same argument to all recursive calls
