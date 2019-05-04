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
--
-- > size empty === 0
empty :: SetMultimap k a
empty = SetMultimap (Map.empty, 0)

-- | /O(1)/. A multimap with a single element.
--
-- > singleton 1 'a' === fromList [(1, 'a')]
-- > size (singleton 1 'a') === 1
singleton :: k -> a -> SetMultimap k a
singleton k a = SetMultimap (Map.singleton k (Set.singleton a), 1)

-- | /O(n*log n)/ where /n/ is the length of the input list.
--  Build a multimap from a list of key\/value pairs.
--
-- > fromList ([] :: [(Int, Char)]) === empty
-- > fromList [(1, 'b'), (2, 'a'), (1, 'b')] === fromList [(1, 'b'), (2, 'a')]
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
--
-- > insert 1 'a' empty === singleton 1 'a'
-- > insert 1 'a' (fromList [(1, 'b'), (2, 'a')]) === fromList [(1, 'a'), (1, 'b'), (2, 'a')]
-- > insert 1 'a' (fromList [(1, 'a'), (2, 'c')]) === fromList [(1, 'a'), (2, 'c')]
insert :: (Ord k, Ord a) => k -> a -> SetMultimap k a -> SetMultimap k a
insert k a (SetMultimap (m, _)) = fromMap' k (Map.alter f k m)
  where
    f (Just as) = Just (Set.insert a as)
    f Nothing = Just (Set.singleton a)

-- | /O(log k)/. Delete a key and all its values from the map.
--
-- > delete 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === singleton 2 'c'
delete :: Ord k => k -> SetMultimap k a -> SetMultimap k a
delete = alter (const Set.empty)

-- | /O(log m * log k)/. Remove the first
-- occurrence of the value associated with the key, if exists.
--
-- > deleteWithValue 1 'c' (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
-- > deleteWithValue 1 'c' (fromList [(2,'c'),(1,'c')]) === singleton 2 'c'
deleteWithValue :: (Ord k, Ord a) => k -> a -> SetMultimap k a -> SetMultimap k a
deleteWithValue k a = alter (Set.delete a) k

-- | /O(log m * log k)/. Remove the maximal value
-- associated with the key, if exists.
--
-- > deleteMax 3 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
-- > deleteMax 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(2,'c')]
deleteMax :: Ord k => k -> SetMultimap k a -> SetMultimap k a
deleteMax = alter Set.deleteMax

-- | /O(log m * log k)/. Remove the minimal value
-- associated with the key, if exists.
--
-- > deleteMin 3 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
-- > deleteMin 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'b'),(2,'c')]
deleteMin :: Ord k => k -> SetMultimap k a -> SetMultimap k a
deleteMin = alter Set.deleteMin

-- | /O(m * log m * log k)/, assuming the function @a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
--
-- Since values are sets, the result may be smaller than the original multimap.
--
-- > adjust ("new " ++) 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"new a"),(1,"new b"),(2,"c")]
-- > adjust (const "z") 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"z"),(2,"c")]
adjust :: (Ord k, Ord a) => (a -> a) -> k -> SetMultimap k a -> SetMultimap k a
adjust = adjustWithKey. const

-- | /O(m * log m * log k)/, assuming the function @k -> a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
--
-- Since values are sets, the result may be smaller than the original multimap.
--
-- > adjustWithKey (\k x -> show k ++ ":new " ++ x) 1 (fromList [(1,"a"),(1,"b"),(2,"c")])
-- >   === fromList [(1,"1:new a"),(1,"1:new b"),(2,"c")]
adjustWithKey :: (Ord k, Ord a) => (k -> a -> a) -> k -> SetMultimap k a -> SetMultimap k a
adjustWithKey f = alterWithKey (Set.map . f)

-- | /O(m * log m * log k)/, assuming the function @a -> 'Maybe' a@
-- takes /O(1)/. The expression (@'update' f k map@) updates the
-- values at key @k@, if exists. If @f@ returns 'Nothing' for a value, the
-- value is deleted.
--
-- > let f x = if x == "a" then Just "new a" else Nothing in do
-- >   update f 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"new a"),(2, "c")]
-- >   update f 1 (fromList [(1,"b"),(1,"c"),(2,"c")]) === singleton 2 "c"
update :: (Ord k, Ord a) => (a -> Maybe a) -> k -> SetMultimap k a -> SetMultimap k a
update = updateWithKey . const

-- | /O(m * log m * log k)/, assuming the function @k -> a -> 'Maybe' a@
-- takes /O(1)/. The expression (@'updateWithKey' f k map@) updates the
-- values at key @k@, if exists. If @f@ returns 'Nothing' for a value, the
-- value is deleted.
--
-- > let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in do
-- >   updateWithKey f 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"1:new a"),(2,"c")]
-- >   updateWithKey f 1 (fromList [(1,"b"),(1,"c"),(2,"c")]) === singleton 2 "c"
updateWithKey :: (Ord k, Ord a) => (k -> a -> Maybe a) -> k -> SetMultimap k a -> SetMultimap k a
updateWithKey f = alterWithKey g
  where
    g k = catMaybes . Set.map (f k)

-- | /O(log k)/, assuming the function @'Set' a -> 'Set' a@ takes /O(1)/.
-- The expression (@'alter' f k map@) alters the values at k, if exists.
--
-- > let (f, g) = (const Set.empty, Set.insert 'c') in do
-- >   alter f 1 (fromList [(1, 'a'), (2, 'b')]) === singleton 2 'b'
-- >   alter f 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b')]
-- >   alter g 1 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'c'), (1, 'a'), (2, 'b')]
-- >   alter g 1 (fromList [(1, 'c'), (2, 'b')]) === fromList [(1, 'c'), (2, 'b')]
-- >   alter g 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b'), (3, 'c')]
alter :: Ord k => (Set a -> Set a) -> k -> SetMultimap k a -> SetMultimap k a
alter = alterWithKey . const

-- | /O(log k)/, assuming the function @k -> 'Set' a -> 'Set' a@ takes /O(1)/.
-- The expression (@'alterWithKey' f k map@) alters the values at k, if exists.
--
-- > let (f, g) = (const (const Set.empty), Set.insert . show) in do
-- >   alterWithKey f 1 (fromList [(1, "a"), (2, "b")]) === singleton 2 "b"
-- >   alterWithKey f 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b")]
-- >   alterWithKey g 1 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "1"), (1, "a"), (2, "b")]
-- >   alterWithKey g 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b"), (3, "3")]
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
--
-- > fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 3 === Set.fromList "ac"
-- > fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 2 === Set.empty
(!) :: Ord k => SetMultimap k a -> k -> Set a
(!) = flip lookup

-- | /O(log k)/. Is the key a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
--
-- > member 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === True
-- > member 1 (deleteMax 1 (fromList [(2, 'c'), (1, 'c')])) === False
member :: Ord k => k -> SetMultimap k a -> Bool
member k (SetMultimap (m, _)) = Map.member k m

-- | /O(log k)/. Is the key not a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
--
-- > notMember 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === False
-- > notMember 1 (deleteMin 1 (fromList [(2, 'c'), (1, 'c')])) === True
notMember :: Ord k => k -> SetMultimap k a -> Bool
notMember k = not . member k

-- | /O(1)/. Is the multimap empty?
--
-- > Data.Multimap.Set.null empty === True
-- > Data.Multimap.Set.null (singleton 1 'a') === False
null :: SetMultimap k a -> Bool
null (SetMultimap (m, _)) = Map.null m

-- | /O(1)/. Is the multimap non-empty?
--
-- > notNull empty === False
-- > notNull (singleton 1 'a') === True
notNull :: SetMultimap k a -> Bool
notNull = not . null

-- | The total number of values for all keys.
--
-- @size@ is evaluated lazily. Forcing the size for the first time takes up to
-- /O(k)/ and subsequent forces take /O(1)/.
--
-- > size empty === 0
-- > size (singleton 1 'a') === 1
-- > size (fromList [(1, 'a'), (2, 'b'), (2, 'c'), (2, 'b')]) === 3
size :: SetMultimap k a -> Int
size (SetMultimap (_, sz)) = sz

------------------------------------------------------------------------------

-- | Union two multimaps, unioning values for duplicate keys.
--
-- > union (fromList [(1,'a'),(2,'b'),(2,'c')]) (fromList [(1,'d'),(2,'b')])
-- >   === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c')]
union :: (Ord k, Ord a) => SetMultimap k a -> SetMultimap k a -> SetMultimap k a
union (SetMultimap (m1, _)) (SetMultimap (m2, _)) =
  fromMap (Map.unionWith Set.union m1 m2)

-- | Union a number of multimaps, unioning values for duplicate keys.
--
-- > unions [fromList [(1,'a'),(2,'b'),(2,'c')], fromList [(1,'d'),(2,'b')]]
-- >   === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c')]
unions :: (Foldable f, Ord k, Ord a) => f (SetMultimap k a) -> SetMultimap k a
unions = Foldable.foldr union empty

-- | Difference of two multimaps.
--
-- If a key exists in the first multimap but not the second, it remains
-- unchanged in the result. If a key exists in both multimaps, a set
-- difference is performed on their values.
--
-- > difference (fromList [(1,'a'),(2,'b'),(2,'c')]) (fromList [(1,'d'),(2,'b'),(2,'a')])
-- >   === fromList [(1,'a'),(2,'c')]
difference :: (Ord k, Ord a) => SetMultimap k a -> SetMultimap k a -> SetMultimap k a
difference (SetMultimap (m1, _)) (SetMultimap (m2, _)) = fromMap $
  Map.differenceWith (\xs ys -> Just (xs Set.\\ ys)) m1 m2

------------------------------------------------------------------------------

-- | /O(n * log m)/, assuming the function @a -> b@ takes /O(1)/.
-- Map a function over all values in the map.
--
-- Since values are sets, the result may be smaller than the original multimap.
--
-- > Data.Multimap.Set.map (++ "x") (fromList [(1,"a"),(2,"b")]) === fromList [(1,"ax"),(2,"bx")]
-- > Data.Multimap.Set.map (const "c") (fromList [(1,"a"),(1,"b"),(2,"b")]) === fromList [(1,"c"),(2,"c")]
map :: Ord b => (a -> b) -> SetMultimap k a -> SetMultimap k b
map = mapWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> b@ takes /O(1)/.
-- Map a function over all values in the map.
--
-- Since values are sets, the result may be smaller than the original multimap.
--
-- > mapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1,"a"),(2,"b")]) === fromList [(1,"1:a"),(2,"2:b")]
mapWithKey :: Ord b => (k -> a -> b) -> SetMultimap k a -> SetMultimap k b
mapWithKey f (SetMultimap (m, _)) = fromMap $ Map.mapWithKey (Set.map . f) m

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator.
--
-- > Data.Multimap.Set.foldr ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldr :: (a -> b -> b) -> b -> SetMultimap k a -> b
foldr = foldrWithKey . const

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator.
--
-- > Data.Multimap.Set.foldl (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldl :: (a -> b -> a) -> a -> SetMultimap k b -> a
foldl = foldlWithKey . (const .)

-- | /O(n)/. Fold the key\/value pairs in the map using the given
-- right-associative binary operator.
--
-- > foldrWithKey (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldrWithKey :: (k -> a -> b -> b) -> b -> SetMultimap k a -> b
foldrWithKey f b (SetMultimap (m, _)) = Map.foldrWithKey f' b m
  where
    f' = flip . Set.foldr . f

-- | /O(n)/. Fold the key\/value pairs in the map using the given
-- left-associative binary operator.
--
-- > foldlWithKey (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldlWithKey :: (a -> k -> b -> a) -> a -> SetMultimap k b -> a
foldlWithKey f a (SetMultimap (m, _)) = Map.foldlWithKey f' a m
  where
    f' = flip (Set.foldl . flip f)

-- | /O(n)/. A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.Set.foldr' ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldr' :: (a -> b -> b) -> b -> SetMultimap k a -> b
foldr' = foldrWithKey' . const

-- | /O(n)/. A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.Set.foldl' (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldl' :: (a -> b -> a) -> a -> SetMultimap k b -> a
foldl' = foldlWithKey' . (const .)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > foldrWithKey' (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldrWithKey' :: (k -> a -> b -> b) -> b -> SetMultimap k a -> b
foldrWithKey' f b (SetMultimap (m, _)) = Map.foldrWithKey f' b m
  where
    f' = flip . Set.foldr' . f

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > foldlWithKey' (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldlWithKey' :: (a -> k -> b -> a) -> a -> SetMultimap k b -> a
foldlWithKey' f a (SetMultimap (m, _)) = Map.foldlWithKey f' a m
  where
    f' = flip (Set.foldl' . flip f)

-- | /O(n)/. Fold the key\/value pairs in the map using the given monoid.
--
-- > foldMapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1, "a"), (1, "c"), (2, "b")]) === "1:a1:c2:b"
foldMapWithKey :: Monoid m => (k -> a -> m) -> SetMultimap k a -> m
foldMapWithKey f (SetMultimap (m, _)) = Map.foldMapWithKey f' m
  where
    f' = Foldable.foldMap . f

------------------------------------------------------------------------------

-- | /O(n)/. Return all elements of the multimap in ascending order of
-- their keys. Elements of each key appear in ascending order.
--
-- > elems (fromList [(2,'a'),(1,'b'),(3,'d'),(3,'c'),(1,'b')]) === "bacd"
-- > elems (empty :: SetMultimap Int Char) === []
elems :: SetMultimap k a -> [a]
elems (SetMultimap (m, _)) = Map.elems m >>= Set.toList

-- | /O(k)/. Return all keys of the multimap in ascending order.
--
-- > keys (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'b')]) === [1,2,3]
-- > keys (empty :: SetMultimap Int Char) === []
keys :: SetMultimap k a -> [k]
keys (SetMultimap (m, _)) = Map.keys m

-- | /O(k)/. The set of all keys of the multimap.
--
-- > keysSet (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'b')]) === Set.fromList [1,2,3]
-- > keysSet (empty :: SetMultimap Int Char) === Set.empty
keysSet :: SetMultimap k a -> Set k
keysSet (SetMultimap (m, _)) = Map.keysSet m

-- | An alias for 'toAscList'.
assocs :: SetMultimap k a -> [(k, a)]
assocs = toAscList

-- | Convert the multimap into a list of key/value pairs.
--
-- > toList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'a'),(1,'b'),(2,'a'),(3,'c')]
toList :: SetMultimap k a -> [(k, a)]
toList = toAscList

-- | Convert the multimap into a list of key/value pairs in ascending
-- order of keys. Elements of each key appear in ascending order.
--
-- > toAscList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'a'),(1,'b'),(2,'a'),(3,'c')]
toAscList :: SetMultimap k a -> [(k, a)]
toAscList (SetMultimap (m, _)) =
  Map.toAscList m >>= uncurry (\k -> fmap (k,) . Set.toAscList)

-- | Convert the multimap into a list of key/value pairs in descending
-- order of keys. Elements of each key appear in descending order.
--
-- > toDescList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(3,'c'),(2,'a'),(1,'b'),(1,'a')]
toDescList :: SetMultimap k a -> [(k, a)]
toDescList (SetMultimap (m, _)) =
  Map.toDescList m >>= uncurry (\k -> fmap (k,) . Set.toDescList)

-- | /O(1)/. Convert the multimap into a regular map.
toMap :: SetMultimap k a -> Map k (Set a)
toMap (SetMultimap (m, _)) = m

------------------------------------------------------------------------------

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all values that satisfy the predicate. A key is removed if
-- none of its values satisfies the predicate.
--
-- > Data.Multimap.Set.filter (> 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === singleton 1 'b'
-- > Data.Multimap.Set.filter (< 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === empty
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
--
-- > filterWithKey (\k a -> even k && a > 'a') (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'b')]) === singleton 2 'b'
filterWithKey :: (k -> a -> Bool) -> SetMultimap k a -> SetMultimap k a
filterWithKey p (SetMultimap (m, _)) = fromMap m'
  where
    m' = Map.mapWithKey (Set.filter . p) m

-- | Generalized 'filter'.
--
-- > let f a | a > 'b' = Just True
-- >         | a < 'b' = Just False
-- >         | a == 'b' = Nothing
-- >  in do
-- >    filterM f (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'c')]) === Nothing
-- >    filterM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Just (fromList [(1,'c'),(2,'c')])
filterM
  :: (Ord k, Ord a, Applicative t)
  => (a -> t Bool) -> SetMultimap k a -> t (SetMultimap k a)
filterM = filterWithKeyM . const

-- | Generalized 'filterWithKey'.
-- | Generalized 'filterWithKey'.
--
-- > let f k a | even k && a > 'b' = Just True
-- >           | odd k && a < 'b' = Just False
-- >           | otherwise = Nothing
-- >  in do
-- >    filterWithKeyM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Nothing
-- >    filterWithKeyM f (fromList [(1,'a'),(3,'a'),(2,'c'),(4,'c')]) === Just (fromList [(2,'c'),(4,'c')])
filterWithKeyM
  :: (Ord k, Ord a, Applicative t)
  => (k -> a -> t Bool) -> SetMultimap k a -> t (SetMultimap k a)
filterWithKeyM f = fmap fromList . List.filterM (uncurry f) . toList

-- | /O(n * log m)/, assuming the function @a -> 'Maybe' b@ takes /O(1)/.
-- Map values and collect the 'Just' results.
--
-- > mapMaybe (\a -> if a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
-- >   === fromList [(1,"new a"),(2,"new a")]
mapMaybe :: Ord b => (a -> Maybe b) -> SetMultimap k a -> SetMultimap k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> 'Maybe' b@ takes /O(1)/.
-- Map key\/value pairs and collect the 'Just' results.
--
-- > mapMaybeWithKey (\k a -> if k > 1 && a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
-- >   === singleton 2 "new a"
mapMaybeWithKey :: Ord b => (k -> a -> Maybe b) -> SetMultimap k a -> SetMultimap k b
mapMaybeWithKey f (SetMultimap (m, _)) = fromMap $
  Map.mapWithKey (\k -> catMaybes . Set.map (f k)) m

-- | /O(n * log m)/, assuming the function @a -> 'Either' b c@ takes /O(1)/.
-- Map values and separate the 'Left' and 'Right' results.
--
-- > mapEither (\a -> if a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
-- >   === (fromList [(1,'a'),(2,'a')],fromList [(1,'c'),(2,'c')])
mapEither
  :: (Ord b, Ord c)
  => (a -> Either b c) -> SetMultimap k a -> (SetMultimap k b, SetMultimap k c)
mapEither = mapEitherWithKey . const

-- | /O(n * log m)/, assuming the function @k -> a -> 'Either' b c@ takes /O(1)/.
-- Map key\/value pairs and separate the 'Left' and 'Right' results.
--
-- > mapEitherWithKey (\k a -> if even k && a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
-- >   === (fromList [(2,'a')],fromList [(1,'a'),(1,'c'),(2,'c')])
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
