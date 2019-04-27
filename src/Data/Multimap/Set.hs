{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
  ) where

import Prelude hiding (filter, foldl, foldr, lookup, map, null)

import           Control.Arrow ((&&&))
import qualified Control.Monad as List (filterM)
import           Data.Data (Data)
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import           Data.Semigroup (Semigroup, (<>))
import           Data.Set (Set)
import qualified Data.Set as Set

infixl 9 !

type Size = Int

newtype SetMultimap k a = SetMultimap (Map k (Set a), Size)
  deriving (Eq, Ord, Data)

instance Eq k => Eq1 (SetMultimap k) where
  liftEq = liftEq2 (==)

instance Eq2 SetMultimap where
  liftEq2 eqk eqv m n =
    Map.size (toMap m) == Map.size (toMap n)
      && liftEq (liftEq2 eqk eqv) (toList m) (toList n)

instance Ord k => Ord1 (SetMultimap k) where
  liftCompare = liftCompare2 compare

instance Ord2 SetMultimap where
  liftCompare2 cmpk cmpv m n =
      liftCompare (liftCompare2 cmpk cmpv) (toList m) (toList n)

instance (Show k, Show a) => Show (SetMultimap k a) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance Show k => Show1 (SetMultimap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 SetMultimap where
  liftShowsPrec2 spk slk spv slv d m =
      showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance (Ord k, Ord a, Read k, Read a) => Read (SetMultimap k a) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    pure (fromList xs,t)

instance Foldable.Foldable (SetMultimap k) where
  foldMap = foldMapWithKey . const
  {-# INLINE foldMap #-}

instance (Ord k, Ord a) => Semigroup (SetMultimap k a) where
  (<>) = union

instance (Ord k, Ord a) => Monoid (SetMultimap k a) where
  mempty = empty
  mappend = (<>)

------------------------------------------------------------------------------

-- | /O(1)/. The empty multimap.
empty :: SetMultimap k a
empty = SetMultimap (Map.empty, 0)

-- | /O(1)/. A multimap with a single element.
singleton :: k -> a -> SetMultimap k a
singleton k a = SetMultimap (Map.singleton k (Set.singleton a), 1)

-- | /O(n*log n)/ where /n/ is the length of the input list.
--  Build a multimap from a list of key\/value pairs.
fromList :: (Ord k, Ord a) => [(k, a)] -> SetMultimap k a
fromList = Foldable.foldr (uncurry insert) empty

-- | /O(k)/. A key is retained only if it is associated with a
-- non-empty set of values.
fromMap :: Map k (Set a) -> SetMultimap k a
fromMap m = SetMultimap (m', sum (fmap Set.size m'))
  where
    m' = Map.filter (not . Set.null) m

------------------------------------------------------------------------------

-- | /O(log m * log k)/. If the key exists in the multimap, the new value will
-- be inserted into the set of values for the key. It is a no-op if the value
-- already exists in the set.
insert :: (Ord k, Ord a) => k -> a -> SetMultimap k a -> SetMultimap k a
insert k a (SetMultimap (m, _)) = fromMap' k (Map.alter f k m)
  where
    f (Just as) = Just (Set.insert a as)
    f Nothing = Just (Set.singleton a)

-- | /O(log k)/. Delete a key and all its values from the map.
delete :: Ord k => k -> SetMultimap k a -> SetMultimap k a
delete = alter (const Set.empty)

-- | /O(log m * log k)/. Remove the first
-- occurrence of the value associated with the key, if exists.
deleteWithValue :: (Ord k, Ord a) => k -> a -> SetMultimap k a -> SetMultimap k a
deleteWithValue k a = alter (Set.delete a) k

-- | /O(log m * log k)/. Remove the maximal value
-- associated with the key, if exists.
deleteMax :: (Ord k, Ord a) => k -> SetMultimap k a -> SetMultimap k a
deleteMax = alter Set.deleteMax

-- | /O(log m * log k)/. Remove the minimal value
-- associated with the key, if exists.
deleteMin :: (Ord k, Ord a) => k -> SetMultimap k a -> SetMultimap k a
deleteMin = alter Set.deleteMin

-- | /O(m * log m * log k)/, assuming the function @a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
adjust :: (Ord k, Ord a) => (a -> a) -> k -> SetMultimap k a -> SetMultimap k a
adjust = adjustWithKey. const

-- | /O(m * log m * log k)/, assuming the function @k -> a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
adjustWithKey :: (Ord k, Ord a) => (k -> a -> a) -> k -> SetMultimap k a -> SetMultimap k a
adjustWithKey f = alterWithKey (Set.map . f)

-- | /O(m * log m * log k)/, assuming the function @a -> 'Maybe' a@
-- takes /O(1)/. The expression (@'update' f k map@) updates the
-- values at key @k@, if exists. If @f@ returns 'Nothing' for a value, the
-- value is deleted.
update :: (Ord k, Ord a) => (a -> Maybe a) -> k -> SetMultimap k a -> SetMultimap k a
update = updateWithKey . const

-- | /O(m * log m * log k)/, assuming the function @k -> a -> 'Maybe' a@
-- takes /O(1)/. The expression (@'updateWithKey' f k map@) updates the
-- values at key @k@, if exists. If @f@ returns 'Nothing' for a value, the
-- value is deleted.
updateWithKey :: (Ord k, Ord a) => (k -> a -> Maybe a) -> k -> SetMultimap k a -> SetMultimap k a
updateWithKey f = alterWithKey g
  where
    g k = catMaybes . Set.map (f k)

-- | /O(log k)/, assuming the function @'Set' a -> 'Set' a@ takes /O(1)/.
-- The expression (@'alter' f k map@) alters the values at k, if exists.
alter :: Ord k => (Set a -> Set a) -> k -> SetMultimap k a -> SetMultimap k a
alter = alterWithKey . const

-- | /O(log k)/, assuming the function @k -> 'Set' a -> 'Set' a@ takes /O(1)/.
-- The expression (@'alterWithKey' f k map@) alters the values at k, if exists.
alterWithKey :: Ord k => (k -> Set a -> Set a) -> k -> SetMultimap k a -> SetMultimap k a
alterWithKey f k mm@(SetMultimap (m, _))
    | Set.null as = fromMap (Map.delete k m)
    | otherwise = fromMap (Map.insert k as m)
  where
    as = f k (mm ! k)

------------------------------------------------------------------------------

-- | /O(log k)/. Lookup the values at a key in the map. It returns an empty
-- set if the key is not in the map.
lookup :: Ord k => k -> SetMultimap k a -> Set a
lookup k (SetMultimap (m, _)) = Maybe.fromMaybe Set.empty (Map.lookup k m)

-- | /O(log k)/. Lookup the values at a key in the map. It returns an empty
-- set if the key is not in the map.
(!) :: Ord k => SetMultimap k a -> k -> Set a
(!) = flip lookup

-- | /O(log k)/. Is the key a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
member :: Ord k => k -> SetMultimap k a -> Bool
member k (SetMultimap (m, _)) = Map.member k m

-- | /O(log k)/. Is the key not a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
notMember :: Ord k => k -> SetMultimap k a -> Bool
notMember k = not . member k

-- | /O(1)/. Is the multimap empty?
null :: SetMultimap k a -> Bool
null (SetMultimap (m, _)) = Map.null m

-- | /O(1)/. Is the multimap non-empty?
notNull :: SetMultimap k a -> Bool
notNull = not . null

-- | The total number of values for all keys.
--
-- @size@ is evaluated lazily. Forcing the size for the first time takes up to
-- /O(k)/ and subsequent forces take /O(1)/.
size :: SetMultimap k a -> Int
size (SetMultimap (_, sz)) = sz

------------------------------------------------------------------------------

-- | Union two multimaps, unioning values for duplicate keys.
union :: (Ord k, Ord a) => SetMultimap k a -> SetMultimap k a -> SetMultimap k a
union (SetMultimap (m1, _)) (SetMultimap (m2, _)) =
  fromMap (Map.unionWith Set.union m1 m2)

-- | Union a number of multimaps, unioning values for duplicate keys.
unions :: (Foldable f, Ord k, Ord a) => f (SetMultimap k a) -> SetMultimap k a
unions = Foldable.foldr union empty

-- | Difference of two multimaps.
--
-- If a key exists in the first multimap but not the second, it remains
-- unchanged in the result. If a key exists in both multimaps, a set
-- difference is performed on their values.
difference :: (Ord k, Ord a) => SetMultimap k a -> SetMultimap k a -> SetMultimap k a
difference (SetMultimap (m1, _)) (SetMultimap (m2, _)) = fromMap $
  Map.differenceWith (\xs ys -> Just (xs Set.\\ ys)) m1 m2

------------------------------------------------------------------------------

-- | /O(n * log m)/, assuming the function @a -> b@ takes /O(1)/.
-- Map a function over all values in the map.
map :: Ord b => (a -> b) -> SetMultimap k a -> SetMultimap k b
map = mapWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> b@ takes /O(1)/.
-- Map a function over all values in the map.
mapWithKey :: Ord b => (k -> a -> b) -> SetMultimap k a -> SetMultimap k b
mapWithKey f (SetMultimap (m, _)) = fromMap $ Map.mapWithKey (Set.map . f) m

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator.
foldr :: (a -> b -> b) -> b -> SetMultimap k a -> b
foldr = foldrWithKey . const

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator.
foldl :: (a -> b -> a) -> a -> SetMultimap k b -> a
foldl = foldlWithKey . (const .)

-- | /O(n)/. Fold the key\/value paris in the map using the given
-- right-associative binary operator.
foldrWithKey :: (k -> a -> b -> b) -> b -> SetMultimap k a -> b
foldrWithKey f b (SetMultimap (m, _)) = Map.foldrWithKey f' b m
  where
    f' = flip . Set.foldr . f

-- | /O(n)/. Fold the key\/value paris in the map using the given
-- left-associative binary operator.
foldlWithKey :: (a -> k -> b -> a) -> a -> SetMultimap k b -> a
foldlWithKey f a (SetMultimap (m, _)) = Map.foldlWithKey f' a m
  where
    f' = flip (Set.foldl . flip f)

-- | /O(n)/. A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> SetMultimap k a -> b
foldr' = foldrWithKey' . const

-- | /O(n)/. A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> SetMultimap k b -> a
foldl' = foldlWithKey' . (const .)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> SetMultimap k a -> b
foldrWithKey' f b (SetMultimap (m, _)) = Map.foldrWithKey f' b m
  where
    f' = flip . Set.foldr' . f

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithKey' :: (a -> k -> b -> a) -> a -> SetMultimap k b -> a
foldlWithKey' f a (SetMultimap (m, _)) = Map.foldlWithKey f' a m
  where
    f' = flip (Set.foldl' . flip f)

-- | /O(n)/. Fold the key\/value pairs in the map using the given monoid.
foldMapWithKey :: Monoid m => (k -> a -> m) -> SetMultimap k a -> m
foldMapWithKey f (SetMultimap (m, _)) = Map.foldMapWithKey f' m
  where
    f' = Foldable.foldMap . f

------------------------------------------------------------------------------

-- | /O(n)/. Return all elements of the multimap in ascending order of
-- their keys.
elems :: SetMultimap k a -> [a]
elems (SetMultimap (m, _)) = Map.elems m >>= Set.toList

-- | /O(k)/. Return all keys of the multimap in ascending order.
keys :: SetMultimap k a -> [k]
keys (SetMultimap (m, _)) = Map.keys m

-- | /O(k)/. The set of all keys of the multimap.
keysSet :: SetMultimap k a -> Set k
keysSet (SetMultimap (m, _)) = Map.keysSet m

-- | An alias for 'toAscList'.
assocs :: SetMultimap k a -> [(k, a)]
assocs = toAscList

-- | Convert the multimap into a list of key/value pairs.
toList :: SetMultimap k a -> [(k, a)]
toList = toAscList

-- | Convert the multimap into a list of key/value pairs in ascending
-- order of keys.
toAscList :: SetMultimap k a -> [(k, a)]
toAscList (SetMultimap (m, _)) =
  Map.toAscList m >>= uncurry (\k -> fmap (k,) . Set.toList)

-- | Convert the multimap into a list of key/value pairs in descending
-- order of keys.
toDescList :: SetMultimap k a -> [(k, a)]
toDescList (SetMultimap (m, _)) =
  Map.toDescList m >>= uncurry (\k -> fmap (k,) . Set.toList)

-- | /O(1)/. Convert the multimap into a regular map.
toMap :: SetMultimap k a -> Map k (Set a)
toMap (SetMultimap (m, _)) = m

------------------------------------------------------------------------------

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all values that satisfy the predicate. A key is removed if
-- none of its values satisfies the predicate.
filter :: (a -> Bool) -> SetMultimap k a -> SetMultimap k a
filter = filterWithKey . const

-- | /O(k)/, assuming the predicate function takes /O(1)/.
-- Retain all keys that satisfy the predicate.
filterKey :: (k -> Bool) -> SetMultimap k a -> SetMultimap k a
filterKey p (SetMultimap (m, _)) = fromMap m'
  where
    m' = Map.filterWithKey (const . p) m

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all key\/value pairs that satisfy the predicate. A key is removed if
-- none of its values satisfies the predicate.
filterWithKey :: (k -> a -> Bool) -> SetMultimap k a -> SetMultimap k a
filterWithKey p (SetMultimap (m, _)) = fromMap m'
  where
    m' = Map.mapWithKey (Set.filter . p) m

-- | Generalized 'filter'.
filterM
  :: (Ord k, Ord a, Applicative t)
  => (a -> t Bool) -> SetMultimap k a -> t (SetMultimap k a)
filterM = filterWithKeyM . const

-- | Generalized 'filterWithKey'.
filterWithKeyM
  :: (Ord k, Ord a, Applicative t)
  => (k -> a -> t Bool) -> SetMultimap k a -> t (SetMultimap k a)
filterWithKeyM f = fmap fromList . List.filterM (uncurry f) . toList

-- | /O(n * log m)/, assuming the function @a -> 'Maybe' b@ takes /O(1)/.
-- Map values and collect the 'Just' results.
mapMaybe :: Ord b => (a -> Maybe b) -> SetMultimap k a -> SetMultimap k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> 'Maybe' b@ takes /O(1)/.
-- Map key\/value pairs and collect the 'Just' results.
mapMaybeWithKey :: Ord b => (k -> a -> Maybe b) -> SetMultimap k a -> SetMultimap k b
mapMaybeWithKey f (SetMultimap (m, _)) = fromMap $
  Map.mapWithKey (\k -> catMaybes . Set.map (f k)) m

-- | /O(n * log m)/, assuming the function @a -> 'Either' b c@ takes /O(1)/.
-- Map values and separate the 'Left' and 'Right' results.
mapEither
  :: (Ord b, Ord c)
  => (a -> Either b c) -> SetMultimap k a -> (SetMultimap k b, SetMultimap k c)
mapEither = mapEitherWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> 'Either' b c@ takes /O(1)/.
-- Map key\/value pairs and separate the 'Left' and 'Right' results.
mapEitherWithKey
  :: (Ord b, Ord c)
  => (k -> a -> Either b c) -> SetMultimap k a -> (SetMultimap k b, SetMultimap k c)
mapEitherWithKey f (SetMultimap (m, _)) =
  (fromMap . Map.mapWithKey (const fst) &&& fromMap . Map.mapWithKey (const snd))
      $ Map.mapWithKey g m
  where
    g k = partitionEithers . Set.map (f k)

------------------------------------------------------------------------------
-- * Non exported functions
------------------------------------------------------------------------------

catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes = Set.foldl' (\s -> maybe s (`Set.insert` s)) Set.empty

partitionEithers :: (Ord a, Ord b) => Set (Either a b) -> (Set a, Set b)
partitionEithers = Set.foldr' (either left right) (Set.empty, Set.empty)
  where
    left a (l,r) = (Set.insert a l, r)
    right b (l,r) = (l, Set.insert b r)

fromMap' :: Ord k => k -> Map k (Set a) -> SetMultimap k a
fromMap' k m = SetMultimap (m', sum (fmap Set.size m'))
  where
    m' = case Map.lookup k m of
      Just as | Set.null as -> Map.delete k m
      _ -> m
