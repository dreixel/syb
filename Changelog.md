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
