-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap.Table
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- The @'Table' r c a@ type represents a finite two-dimensional table
-- that associates a pair of keys (a row key of type @r@ and
-- a column key of type @c@) with a value of type @a@.
--
-- The implementation is backed by two maps: a @'Map' r ('Map' c) a@, and
-- a @'Map' c ('Map' r) a@, called "row map" and "column map", respectively.
--
-- It is worth noting that all functions that traverse a table, such as
-- 'foldl', 'foldr', 'foldMap' and 'traverse', are row-oriented, i.e.,
-- they traverse the table row by row. To traverse a table column
-- by column, 'transpose' the table first.
--
-- In the following Big-O notations, unless otherwise noted, /n/ denotes
-- the size of the table (i.e., the total number of values for all
-- row and column keys), /r/ denotes the number of row keys that has at
-- least one value, /c/ denotes the number of column keys that has at
-- least one value, and /k = max r c/.
module Data.Multimap.Table (
  Table

  -- * Construction
  , empty
  , singleton
  , fromRowMap
  , fromColumnMap
  , transpose

  -- ** From Unordered Lists
  , fromList

  -- * Deletion\/Update
  , insert
  , delete
  , deleteRow
  , deleteColumn
  , adjust
  , adjustWithKeys
  , update
  , updateWithKeys
  , alter
  , alterWithKeys

  -- * Query
  -- ** Lookup
  , lookup
  , (!?)
  , (!)
  , hasCell
  , hasRow
  , hasColumn

  -- ** Size
  , null
  , notNull
  , size

  -- * Combine
  -- ** Union
  , union
  , unionWith
  , unionWithKeys
  , unions
  , unionsWith
  , unionsWithKeys

  -- ** Difference
  , difference

  -- * Traversal
  -- ** Map
  , map
  , mapWithKeys
  , traverseWithKeys
  , traverseMaybeWithKeys

  -- ** Folds
  , foldr
  , foldl
  , foldrWithKeys
  , foldlWithKeys
  , foldMapWithKeys

  -- ** Strict Folds
  , foldr'
  , foldl'
  , foldrWithKeys'
  , foldlWithKeys'

  -- * Conversion
  , row
  , column
  , rowMap
  , columnMap
  , rowKeys
  , columnKeys
  , rowKeysSet
  , columnKeysSet

  -- ** Lists
  , toList

  -- ** Ordered lists
  , toRowAscList
  , toColumnAscList
  , toRowDescList
  , toColumnDescList

  -- * Filter
  , filter
  , filterRow
  , filterColumn
  , filterWithKeys

  , mapMaybe
  , mapMaybeWithKeys
  , mapEither
  , mapEitherWithKeys
  ) where

import Data.Multimap.Table.Internal
import Prelude hiding (Foldable(..), filter, lookup, map)
