## Release v0.17.0

- Added `Parsexp.Conv_many_and_locations`

- In eager parser, add an option `reraise_notrace` to better support call sites
    that use exceptions as control-flow.

## Release v0.16.0

- Expose type equalities for `State.t` and `Stack.t` types in off-the-shelf parser modules.
  This lets you use one of those modules, but without giving up the flexibility of interaction with
  the parser state.

## Old pre-v0.15 changelogs (very likely stale and incomplete)

# v0.11

Drop dependency on Base.

# v0.10

Initial release
