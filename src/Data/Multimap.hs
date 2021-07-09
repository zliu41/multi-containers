-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Multimaps, where values behave like (non empty) lists.
--
-- Multimaps whose values behave like sets are in "Data.Multimap.Set".
-- Multimaps whose values behave like maps (i.e., two-dimensional
-- tables) are in "Data.Multimap.Table".
--
-- The implementation is backed by a @'Map' k ('NonEmpty' a)@. The
-- differences between @'Multimap' k a@ and @'Map' k ('NonEmpty' a)@ include:
--
--   * 'lookup' (or '!') returns a possibly empty list. Unlike regular maps,
--     the '!' operator is total for multimaps.
--
--   * Functions like 'map', 'adjust', 'traverse', etc., take functions on
--     individual values (e.g., @a -> b@) as opposed to, e.g.,
--     @'NonEmpty' a -> 'NonEmpty' b@.
--
--   * 'union' and 'unions' concatenate the values when there are duplicate
--     keys, rather than being left- or right-biased.
--
--   * The 'difference' function computes list differences for values of
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
module Data.Multimap (
  -- * Multimap type
  Multimap

  -- * Construction
  , empty
  , singleton
  , fromMap
  , fromMap'

  -- ** From Unordered Lists
  , fromList

  -- * Insertion
  , insert

  -- * Deletion\/Update
  , delete
  , deleteWithValue
  , deleteOne
  , adjust
  , adjustWithKey
  , update
  , update'
  , updateWithKey
  , updateWithKey'
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
  , traverseWithKey
  , traverseMaybeWithKey

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
  , toAscListBF
  , toDescListBF

  -- ** Maps
  , toMap

  -- ** SetMultimaps
  , fromSetMultimapAsc
  , fromSetMultimapDesc
  , toSetMultimap

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

import Data.Multimap.Conversion
import Data.Multimap.Internal
import Data.Multimap.Set.Internal (SetMultimap)
import Prelude hiding (filter, foldl, foldr, lookup, map, null)

-- | Convert a t'Data.Multimap.Set.SetMultimap' to a t'Data.Multimap.Multimap' where the values of each key
-- are in ascending order.
--
-- > fromSetMultimapAsc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]
fromSetMultimapAsc :: SetMultimap k a -> Multimap k a
fromSetMultimapAsc = toMultimapAsc

-- | Convert a t'Data.Multimap.Set.SetMultimap' to a t'Data.Multimap.Multimap' where the values of each key
-- are in descending order.
--
-- > fromSetMultimapDesc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'b'),(1,'a'),(2,'c')]
fromSetMultimapDesc :: SetMultimap k a -> Multimap k a
fromSetMultimapDesc = toMultimapDesc
