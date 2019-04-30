# multi-containers

A library that provides three multimap variants:

- [`Multimap k a`](https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap.html), a multimap where values of each key form a list.
- [`SetMultimap k a`](https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap-Set.html), a multimap where values of each key form a set.
- [`Table r c a`](https://hackage.haskell.org/package/multi-containers/docs/Data-Multimap-Table.html), a two dimensional table where
  values are indexed by row keys and column keys; it is essentially a multimap where values of each key form a map.

For `Multimap` and `SetMultimap`, there's always at least one value associated with each key in the multimap. Upon removing
the last value of a key, the key itself is removed from the multimap.

For `Table`, similarly, each row key in a table always has at least one value. So does each column key.
