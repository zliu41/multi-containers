-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap.Set
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Multimaps, where values behave like (non empty) sets.
--
-- Multimaps whose values behave like lists are in "Data.Multimap".
-- Multimaps whose values behave like maps (i.e., two-dimensional
-- tables) are in "Data.Multimap.Table".
--
-- The implementation is backed by a @'Map' k ('Set' a)@. The
-- differences between @'Multimap' k a@ and @'Map' k ('Set' a)@ include:
--
--   * A key is only present in a 'SetMultimap' if it is associated with
--     at least one values, i.e., a key is never associated with an empty
--     set of values.
--
--   * 'lookup' (or '!') returns a possibly empty set. Unlike regular maps,
--     the '!' operator is total for multimaps.
--
--   * Functions like 'map', 'adjust', 'update', etc., take functions on
--     individual values (e.g., @a -> b@) as opposed to, e.g.,
--     @'Set' a -> 'Set' b@.
--
--   * 'union' and 'unions' union the values when there are duplicate
--     keys, rather than being left- or right-biased.
--
--   * The 'difference' function computes set differences for values of
--     keys that exist in both maps.
--
--   * The 'size' function returns the total number of values for all keys in
--     the multimap, not the number of distinct keys. The latter can be obtained
--     by first getting the 'keysSet' or first converting the multimap to
--     a regular map via 'toMap'.
--
-- In the following Big-O notations, unless otherwise noted, /n/ denotes
-- the size of the multimap, /k/ denotes the number of distinct keys, and
-- /m/ denotes the maximum number of values associated with a single key.
module Data.Multimap.Set (
  -- * Multimap type
  SetMultimap

  -- * Construction
  , empty
  , singleton
  , fromMap

  -- ** From Unordered Lists
  , fromList

  -- * Insertion
  , insert

  -- * Deletion\/Update
  , delete
  , deleteWithValue
  , deleteMax
  , deleteMin
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , alter
  , alterWithKey

  -- * Query
  -- ** Lookup
  , lookup
  , (!)
  , member
  , notMember

  -- ** Size
  , null
  , notNull
  , size

  -- * Combine
  -- ** Union
  , union
  , unions

  -- ** Difference
  , difference

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey

  -- ** Folds
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey

  -- ** Strict Folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'

  -- * Conversion
  , elems
  , keys
  , assocs
  , keysSet

  -- ** Lists
  , toList

  -- ** Ordered lists
  , toAscList
  , toDescList

  -- ** Maps
  , toMap

  -- * Filter
  , filter
  , filterWithKey
  , filterKey
  , filterM
  , filterWithKeyM

  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey

  -- * Min\/Max
  , lookupMin
  , lookupMax
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  ) where

import Data.Multimap.Set.Internal
import Prelude hiding (filter, foldl, foldr, lookup, map, null)
