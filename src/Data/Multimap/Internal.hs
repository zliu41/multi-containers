{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap.Internal
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
module Data.Multimap.Internal (
  -- * Multimap type
  Multimap (..)
  , Size

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

import           Control.Arrow ((&&&))
import           Control.Monad (join)
import qualified Control.Monad as List (filterM)
import           Data.Data (Data)
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as Nel
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)

import Prelude hiding (filter, foldl, foldr, lookup, map, null)

infixl 9 !

type Size = Int

newtype Multimap k a = Multimap (Map k (NonEmpty a), Size)
  deriving (Eq, Ord, Data)

instance Eq k => Eq1 (Multimap k) where
  liftEq = liftEq2 (==)

instance Eq2 Multimap where
  liftEq2 eqk eqv m n =
    Map.size (toMap m) == Map.size (toMap n)
      && liftEq (liftEq2 eqk eqv) (toList m) (toList n)

instance Ord k => Ord1 (Multimap k) where
  liftCompare = liftCompare2 compare

instance Ord2 Multimap where
  liftCompare2 cmpk cmpv m n =
      liftCompare (liftCompare2 cmpk cmpv) (toList m) (toList n)

instance (Show k, Show a) => Show (Multimap k a) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance Show k => Show1 (Multimap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 Multimap where
  liftShowsPrec2 spk slk spv slv d m =
      showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance (Ord k, Read k, Read e) => Read (Multimap k e) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    pure (fromList xs,t)

instance (Ord k, Read k) => Read1 (Multimap k) where
  liftReadsPrec rp rl = readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance Functor (Multimap k) where
  fmap = map

instance Foldable.Foldable (Multimap k) where
  foldMap = foldMapWithKey . const
  {-# INLINE foldMap #-}

instance Traversable (Multimap k) where
  traverse = traverseWithKey . const
  {-# INLINE traverse #-}

instance (Ord k) => Semigroup (Multimap k a) where
  (<>) = union

instance (Ord k) => Monoid (Multimap k a) where
  mempty = empty
  mappend = (<>)

------------------------------------------------------------------------------

-- | /O(1)/. The empty multimap.
--
-- > size empty === 0
empty :: Multimap k a
empty = Multimap (Map.empty, 0)

-- | /O(1)/. A multimap with a single element.
--
-- > singleton 1 'a' === fromList [(1, 'a')]
-- > size (singleton 1 'a') === 1
singleton :: k -> a -> Multimap k a
singleton k a = Multimap (Map.singleton k (pure a), 1)

-- | /O(n*log n)/ where /n/ is the length of the input list.
--  Build a multimap from a list of key\/value pairs.
--
-- > fromList ([] :: [(Int, Char)]) === empty
fromList :: Ord k => [(k, a)] -> Multimap k a
fromList = Foldable.foldr (uncurry insert) empty

-- | /O(1)/.
fromMap :: Map k (NonEmpty a) -> Multimap k a
fromMap m = Multimap (m, sum (fmap length m))

-- | /O(k)/. A key is retained only if it is associated with a
-- non-empty list of values.
--
-- > fromMap' (Map.fromList [(1, "ab"), (2, ""), (3, "c")]) === fromList [(1, 'a'), (1, 'b'), (3, 'c')]
fromMap' :: Map k [a] -> Multimap k a
fromMap' m = Multimap (Map.mapMaybe nonEmpty m, sum (fmap length m))

------------------------------------------------------------------------------

-- | /O(log k)/. If the key exists in the multimap, the new value will be
-- prepended to the list of values for the key.
--
-- > insert 1 'a' empty === singleton 1 'a'
-- > insert 1 'a' (fromList [(2, 'b'), (2, 'c')]) === fromList [(1, 'a'), (2, 'b'), (2, 'c')]
-- > insert 1 'a' (fromList [(1, 'b'), (2, 'c')]) === fromList [(1, 'a'), (1, 'b'), (2, 'c')]
insert :: Ord k => k -> a -> Multimap k a -> Multimap k a
insert k a (Multimap (m, _)) = fromMap (Map.alter f k m)
  where
    f (Just as) = Just (a <| as)
    f Nothing = Just (pure a)

-- | /O(log k)/. Delete a key and all its values from the map.
--
-- > delete 1 (fromList [(1, 'a'), (1, 'b'), (2, 'c')]) === singleton 2 'c'
delete :: Ord k => k -> Multimap k a -> Multimap k a
delete = update' (const [])

-- | /O(m*log k)/. Remove the first
-- occurrence of the value associated with the key, if exists.
--
-- > deleteWithValue 1 'c' (fromList [(1, 'a'), (1, 'b'), (2, 'c')]) === fromList [(1, 'a'), (1, 'b'), (2, 'c')]
-- > deleteWithValue 1 'c' (fromList [(1, 'a'), (1, 'b'), (2, 'c'), (1, 'c')]) === fromList [(1, 'a'), (1, 'b'), (2, 'c')]
-- > deleteWithValue 1 'c' (fromList [(2, 'c'), (1, 'c')]) === singleton 2 'c'
deleteWithValue :: (Ord k, Eq a) => k -> a -> Multimap k a -> Multimap k a
deleteWithValue k a = update' (List.delete a . Nel.toList) k

-- | /O(log k)/. Remove the first
-- value associated with the key. If the key is associated with a single value,
-- the key will be removed from the multimap.
--
-- > deleteOne 1 (fromList [(1, 'a'), (1, 'b'), (2, 'c')]) === fromList [(1, 'b'), (2, 'c')]
-- > deleteOne 1 (fromList [(2, 'c'), (1, 'c')]) === singleton 2 'c'
deleteOne :: Ord k => k -> Multimap k a -> Multimap k a
deleteOne = update' Nel.tail

-- | /O(m*log k)/, assuming the function @a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
--
-- > adjust ("new " ++) 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"new a"),(1,"new b"),(2,"c")]
adjust :: Ord k => (a -> a) -> k -> Multimap k a -> Multimap k a
adjust = adjustWithKey . const

-- | /O(m*log k)/, assuming the function @k -> a -> a@ takes /O(1)/.
-- Update values at a specific key, if exists.
--
-- > adjustWithKey (\k x -> show k ++ ":new " ++ x) 1 (fromList [(1,"a"),(1,"b"),(2,"c")])
-- >   === fromList [(1,"1:new a"),(1,"1:new b"),(2,"c")]
adjustWithKey :: Ord k => (k -> a -> a) -> k -> Multimap k a -> Multimap k a
adjustWithKey f k (Multimap (m, sz)) = Multimap (m', sz)
  where
    m' = Map.adjustWithKey (fmap . f) k m

-- | /O(m*log k)/, assuming the function @a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'update' f k map@) updates the values at key @k@, if
-- exists. If @f@ returns 'Nothing' for a value, the value is deleted.
--
-- > let f x = if x == "a" then Just "new a" else Nothing in do
-- >   update f 1 (fromList [(1,"a"),(1, "b"),(2,"c")]) === fromList [(1,"new a"),(2, "c")]
-- >   update f 1 (fromList [(1,"b"),(1, "b"),(2,"c")]) === singleton 2 "c"
update :: Ord k => (a -> Maybe a) -> k -> Multimap k a -> Multimap k a
update = updateWithKey . const

-- | /O(log k)/, assuming the function @'NonEmpty' a -> [a]@ takes /O(1)/.
-- The expression (@'update' f k map@) updates the values at key @k@, if
-- exists. If @f@ returns 'Nothing', the key is deleted.
--
-- > update' NonEmpty.tail 1 (fromList [(1, "a"), (1, "b"), (2, "c")]) === fromList [(1, "b"), (2, "c")]
-- > update' NonEmpty.tail 1 (fromList [(1, "a"), (2, "b")]) === singleton 2 "b"
update' :: Ord k => (NonEmpty a -> [a]) -> k -> Multimap k a -> Multimap k a
update' = updateWithKey' . const

-- | /O(m*log k)/, assuming the function @k -> a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'updateWithKey' f k map@) updates the values at key @k@, if
-- exists. If @f@ returns 'Nothing' for a value, the value is deleted.
--
-- > let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in do
-- >   updateWithKey f 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"1:new a"),(2,"c")]
-- >   updateWithKey f 1 (fromList [(1,"b"),(1,"b"),(2,"c")]) === singleton 2 "c"
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Multimap k a -> Multimap k a
updateWithKey f = alterWithKey (Maybe.mapMaybe . f)

-- | /O(log k)/, assuming the function @k -> 'NonEmpty' a -> [a]@ takes /O(1)/.
-- The expression (@'update' f k map@) updates the values at key @k@, if
-- exists. If @f@ returns 'Nothing', the key is deleted.
--
-- > let f k xs = if NonEmpty.length xs == 1 then (show k : NonEmpty.toList xs) else [] in do
-- >   updateWithKey' f 1 (fromList [(1, "a"), (1, "b"), (2, "c")]) === singleton 2 "c"
-- >   updateWithKey' f 1 (fromList [(1, "a"), (2, "b"), (2, "c")]) === fromList [(1, "1"), (1, "a"), (2, "b"), (2, "c")]
updateWithKey' :: Ord k => (k -> NonEmpty a -> [a]) -> k -> Multimap k a -> Multimap k a
updateWithKey' f = alterWithKey g
  where
    g _ [] = []
    g k (a:as) = f k (a :| as)

-- | /O(log k)/, assuming the function @[a] -> [a]@ takes /O(1)/.
-- The expression (@'alter' f k map@) alters the values at k, if exists.
--
-- > let (f, g) = (const [], ('c':)) in do
-- >   alter f 1 (fromList [(1, 'a'), (2, 'b')]) === singleton 2 'b'
-- >   alter f 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b')]
-- >   alter g 1 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'c'), (1, 'a'), (2, 'b')]
-- >   alter g 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b'), (3, 'c')]
alter :: Ord k => ([a] -> [a]) -> k -> Multimap k a -> Multimap k a
alter = alterWithKey . const

-- | /O(log k)/, assuming the function @k -> [a] -> [a]@ takes /O(1)/.
-- The expression (@'alterWithKey' f k map@) alters the values at k, if exists.
--
-- > let (f, g) = (const (const []), (:) . show) in do
-- >   alterWithKey f 1 (fromList [(1, "a"), (2, "b")]) === singleton 2 "b"
-- >   alterWithKey f 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b")]
-- >   alterWithKey g 1 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "1"), (1, "a"), (2, "b")]
-- >   alterWithKey g 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b"), (3, "3")]
alterWithKey :: Ord k => (k -> [a] -> [a]) -> k -> Multimap k a -> Multimap k a
alterWithKey f k mm@(Multimap (m, _)) = case nonEmpty (f k (mm ! k)) of
    Just as' -> fromMap (Map.insert k as' m)
    Nothing -> fromMap (Map.delete k m)

------------------------------------------------------------------------------

-- | /O(log k)/. Lookup the values at a key in the map. It returns an empty
-- list if the key is not in the map.
lookup :: Ord k => k -> Multimap k a -> [a]
lookup k (Multimap (m, _)) = maybe [] Nel.toList (Map.lookup k m)

-- | /O(log k)/. Lookup the values at a key in the map. It returns an empty
-- list if the key is not in the map.
--
-- > fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 3 === "ac"
-- > fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 2 === []
(!) :: Ord k => Multimap k a -> k -> [a]
(!) = flip lookup

-- | /O(log k)/. Is the key a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
--
-- > member 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === True
-- > member 1 (deleteOne 1 (fromList [(2, 'c'), (1, 'c')])) === False
member :: Ord k => k -> Multimap k a -> Bool
member k (Multimap (m, _)) = Map.member k m

-- | /O(log k)/. Is the key not a member of the map?
--
-- A key is a member of the map if and only if there is at least one value
-- associated with it.
--
-- > notMember 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === False
-- > notMember 1 (deleteOne 1 (fromList [(2, 'c'), (1, 'c')])) === True
notMember :: Ord k => k -> Multimap k a -> Bool
notMember k = not . member k

-- | /O(1)/. Is the multimap empty?
--
-- > Data.Multimap.null empty === True
-- > Data.Multimap.null (singleton 1 'a') === False
null :: Multimap k a -> Bool
null (Multimap (m, _)) = Map.null m

-- | /O(1)/. Is the multimap non-empty?
--
-- > notNull empty === False
-- > notNull (singleton 1 'a') === True
notNull :: Multimap k a -> Bool
notNull = not . null

-- | The total number of values for all keys.
--
-- @size@ is evaluated lazily. Forcing the size for the first time takes up to
-- /O(n)/ and subsequent forces take /O(1)/.
--
-- > size empty === 0
-- > size (singleton 1 'a') === 1
-- > size (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === 3
size :: Multimap k a -> Int
size (Multimap (_, sz)) = sz

------------------------------------------------------------------------------

-- | Union two multimaps, concatenating values for duplicate keys.
--
-- > union (fromList [(1,'a'),(2,'b'),(2,'c')]) (fromList [(1,'d'),(2,'b')])
-- >   === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c'),(2,'b')]
union :: Ord k => Multimap k a -> Multimap k a -> Multimap k a
union (Multimap (m1, _)) (Multimap (m2, _)) =
  fromMap (Map.unionWith (<>) m1 m2)

-- | Union a number of multimaps, concatenating values for duplicate keys.
--
-- > unions [fromList [(1,'a'),(2,'b'),(2,'c')], fromList [(1,'d'),(2,'b')]]
-- >   === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c'),(2,'b')]
unions :: (Foldable f, Ord k) => f (Multimap k a) -> Multimap k a
unions = Foldable.foldr union empty

-- | Difference of two multimaps.
--
-- If a key exists in the first multimap but not the second, it remains
-- unchanged in the result. If a key exists in both multimaps, a list
-- difference is performed on their values, i.e., the first occurrence
-- of each value in the second multimap is removed from the
-- first multimap.
--
-- > difference (fromList [(1,'a'),(2,'b'),(2,'c'),(2,'b')]) (fromList [(1,'d'),(2,'b'),(2,'a')])
-- >   === fromList [(1,'a'), (2,'c'), (2,'b')]
difference :: (Ord k, Eq a) => Multimap k a -> Multimap k a -> Multimap k a
difference (Multimap (m1, _)) (Multimap (m2, _)) = fromMap $
  Map.differenceWith (\xs ys -> nonEmpty (Nel.toList xs List.\\ Nel.toList ys)) m1 m2

------------------------------------------------------------------------------

-- | /O(n)/, assuming the function @a -> b@ takes /O(1)/.
-- Map a function over all values in the map.
--
-- > Data.Multimap.map (++ "x") (fromList [(1,"a"),(1,"a"),(2,"b")]) === fromList [(1,"ax"),(1,"ax"),(2,"bx")]
map :: (a -> b) -> Multimap k a -> Multimap k b
map = mapWithKey . const

-- | /O(n)/, assuming the function @k -> a -> b@ takes /O(1)/.
-- Map a function over all key\/value pairs in the map.
--
-- > mapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1,"a"),(1,"a"),(2,"b")]) === fromList [(1,"1:a"),(1,"1:a"),(2,"2:b")]
mapWithKey :: (k -> a -> b) -> Multimap k a -> Multimap k b
mapWithKey f (Multimap (m, sz)) = Multimap (Map.mapWithKey (fmap . f) m, sz)

-- | Traverse key\/value pairs and collect the results.
--
-- > let f k a = if odd k then Just (succ a) else Nothing in do
-- >   traverseWithKey f (fromList [(1, 'a'), (1, 'b'), (3, 'b'), (3, 'c')]) === Just (fromList [(1, 'b'), (1, 'c'), (3, 'c'), (3, 'd')])
-- >   traverseWithKey f (fromList [(1, 'a'), (1, 'b'), (2, 'b')]) === Nothing
traverseWithKey :: Applicative t => (k -> a -> t b) -> Multimap k a -> t (Multimap k b)
traverseWithKey f (Multimap (m, _)) =
  fromMap <$> Map.traverseWithKey (traverse . f) m

-- | Traverse key\/value pairs and collect the 'Just' results.
traverseMaybeWithKey :: Applicative t => (k -> a -> t (Maybe b)) -> Multimap k a -> t (Multimap k b)
traverseMaybeWithKey f (Multimap (m, _)) =
    fromMap <$> Map.traverseMaybeWithKey f' m
  where
    f' k = fmap (nonEmpty . Maybe.catMaybes) . traverse (f k) . Nel.toList

------------------------------------------------------------------------------

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator.
--
-- > Data.Multimap.foldr ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldr :: (a -> b -> b) -> b -> Multimap k a -> b
foldr = foldrWithKey . const

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator.
--
-- > Data.Multimap.foldl (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldl :: (a -> b -> a) -> a -> Multimap k b -> a
foldl = foldlWithKey . (const .)

-- | /O(n)/. Fold the key\/value pairs in the map using the given
-- right-associative binary operator.
--
-- > foldrWithKey (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldrWithKey :: (k -> a -> b -> b) -> b -> Multimap k a -> b
foldrWithKey f b (Multimap (m, _)) = Map.foldrWithKey f' b m
  where
    f' = flip . Foldable.foldr . f

-- | /O(n)/. Fold the key\/value pairs in the map using the given
-- left-associative binary operator.
--
-- > foldlWithKey (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldlWithKey :: (a -> k -> b -> a) -> a -> Multimap k b -> a
foldlWithKey f a (Multimap (m, _)) = Map.foldlWithKey f' a m
  where
    f' = flip (Foldable.foldl . flip f)

-- | /O(n)/. A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.foldr' ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldr' :: (a -> b -> b) -> b -> Multimap k a -> b
foldr' = foldrWithKey' . const

-- | /O(n)/. A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.foldl' (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
foldl' :: (a -> b -> a) -> a -> Multimap k b -> a
foldl' = foldlWithKey' . (const .)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > foldrWithKey' (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldrWithKey' :: (k -> a -> b -> b) -> b -> Multimap k a -> b
foldrWithKey' f b (Multimap (m, _)) = Map.foldrWithKey' f' b m
  where
    f' = flip . Foldable.foldr . f

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > foldlWithKey' (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
foldlWithKey' :: (a -> k -> b -> a) -> a -> Multimap k b -> a
foldlWithKey' f a (Multimap (m, _)) = Map.foldlWithKey' f' a m
  where
    f' = flip (Foldable.foldl' . flip f)

-- | /O(n)/. Fold the key\/value pairs in the map using the given monoid.
--
-- > foldMapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1, "a"), (1, "a"), (2, "b")]) === "1:a1:a2:b"
foldMapWithKey :: Monoid m => (k -> a -> m) -> Multimap k a -> m
foldMapWithKey f (Multimap (m, _)) = Map.foldMapWithKey f' m
  where
    f' = Foldable.foldMap . f

------------------------------------------------------------------------------

-- | /O(n)/. Return all elements of the multimap in ascending order of
-- their keys.
--
-- > elems (fromList [(2, 'a'), (1, 'b'), (3, 'c'), (1, 'b')]) === "bbac"
-- > elems (empty :: Multimap Int Char) === []
elems :: Multimap k a -> [a]
elems (Multimap (m, _)) = Map.elems m >>= Nel.toList

-- | /O(k)/. Return all keys of the multimap in ascending order.
--
-- > keys (fromList [(2, 'a'), (1, 'b'), (3, 'c'), (1, 'b')]) === [1,2,3]
-- > keys (empty :: Multimap Int Char) === []
keys :: Multimap k a -> [k]
keys (Multimap (m, _)) = Map.keys m

-- | /O(k)/. The set of all keys of the multimap.
--
-- > keysSet (fromList [(2, 'a'), (1, 'b'), (3, 'c'), (1, 'b')]) === Set.fromList [1,2,3]
-- > keysSet (empty :: Multimap Int Char) === Set.empty
keysSet :: Multimap k a -> Set k
keysSet (Multimap (m, _)) = Map.keysSet m

-- | An alias for 'toAscList'.
--
-- > assocs (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'b'),(1,'a'),(2,'a'),(3,'c')]
assocs :: Multimap k a -> [(k, a)]
assocs = toAscList

-- | Convert the multimap into a list of key/value pairs.
--
-- > toList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'b'),(1,'a'),(2,'a'),(3,'c')]
toList :: Multimap k a -> [(k, a)]
toList = toAscList

-- | Convert the multimap into a list of key/value pairs in ascending
-- order of keys.
--
-- > toAscList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'b'),(1,'a'),(2,'a'),(3,'c')]
toAscList :: Multimap k a -> [(k, a)]
toAscList (Multimap (m, _)) =
  Map.toAscList m >>= uncurry (\k -> fmap (k,) . Nel.toList)

-- | Convert the multimap into a list of key/value pairs in descending
-- order of keys.
--
-- > toDescList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(3,'c'),(2,'a'),(1,'b'),(1,'a')]
toDescList :: Multimap k a -> [(k, a)]
toDescList (Multimap (m, _)) =
  Map.toDescList m >>= uncurry (\k -> fmap (k,) . Nel.toList)

-- | Convert the multimap into a list of key/value pairs, in a
-- breadth-first manner, in ascending order of keys.
--
-- > toAscListBF (fromList [("Foo",1),("Foo",2),("Foo",3),("Bar",4),("Bar",5),("Baz",6)])
-- >   === [("Bar",4),("Baz",6),("Foo",1),("Bar",5),("Foo",2),("Foo",3)]
toAscListBF :: Multimap k a -> [(k, a)]
toAscListBF (Multimap (m, _)) =
  join
  . List.transpose
  . fmap (uncurry (\k -> fmap (k,) . Nel.toList))
  $ Map.toAscList m

-- | Convert the multimap into a list of key/value pairs, in a
-- breadth-first manner, in descending order of keys.
--
-- > toDescListBF (fromList [("Foo",1),("Foo",2),("Foo",3),("Bar",4),("Bar",5),("Baz",6)])
-- >   === [("Foo",1),("Baz",6),("Bar",4),("Foo",2),("Bar",5),("Foo",3)]
toDescListBF :: Multimap k a -> [(k, a)]
toDescListBF (Multimap (m, _)) =
  join
  . List.transpose
  . fmap (uncurry (\k -> fmap (k,) . Nel.toList))
  $ Map.toDescList m

-- | /O(1)/. Convert the multimap into a regular map.
toMap :: Multimap k a -> Map k (NonEmpty a)
toMap (Multimap (m, _)) = m

------------------------------------------------------------------------------

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all values that satisfy the predicate.
--
-- > Data.Multimap.filter (> 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === singleton 1 'b'
-- > Data.Multimap.filter (< 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === empty
filter :: (a -> Bool) -> Multimap k a -> Multimap k a
filter = filterWithKey . const

-- | /O(k)/, assuming the predicate function takes /O(1)/.
-- Retain all keys that satisfy the predicate.
--
-- > filterKey even (fromList [(1,'a'),(1,'b'),(2,'a')]) === singleton 2 'a'
filterKey :: (k -> Bool) -> Multimap k a -> Multimap k a
filterKey p (Multimap (m, _)) = fromMap m'
  where
    m' = Map.filterWithKey (const . p) m

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all key\/value pairs that satisfy the predicate.
--
-- > filterWithKey (\k a -> even k && a > 'a') (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'b')]) === singleton 2 'b'
filterWithKey :: (k -> a -> Bool) -> Multimap k a -> Multimap k a
filterWithKey p (Multimap (m, _)) = fromMap m'
  where
    m' = Map.mapMaybeWithKey (\k -> nonEmpty . Nel.filter (p k)) m

-- | Generalized 'filter'.
--
-- > let f a | a > 'b' = Just True
-- >         | a < 'b' = Just False
-- >         | a == 'b' = Nothing
-- >  in do
-- >    filterM f (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'c')]) === Nothing
-- >    filterM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Just (fromList [(1,'c'),(2,'c')])
filterM :: (Ord k, Applicative t) => (a -> t Bool) -> Multimap k a -> t (Multimap k a)
filterM = filterWithKeyM . const

-- | Generalized 'filterWithKey'.
--
-- > let f k a | even k && a > 'b' = Just True
-- >           | odd k && a < 'b' = Just False
-- >           | otherwise = Nothing
-- >  in do
-- >    filterWithKeyM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Nothing
-- >    filterWithKeyM f (fromList [(1,'a'),(1,'a'),(2,'c'),(2,'c')]) === Just (fromList [(2,'c'),(2,'c')])
filterWithKeyM :: (Ord k, Applicative t) => (k -> a -> t Bool) -> Multimap k a -> t (Multimap k a)
filterWithKeyM f = fmap fromList . List.filterM (uncurry f) . toList

-- | /O(n)/, assuming the function @a -> 'Maybe' b@ takes /O(1)/.
-- Map values and collect the 'Just' results.
--
-- > mapMaybe (\a -> if a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
-- >   === fromList [(1,"new a"),(2,"new a")]
mapMaybe :: (a -> Maybe b) -> Multimap k a -> Multimap k b
mapMaybe = mapMaybeWithKey . const

-- | /O(n)/, assuming the function @k -> a -> 'Maybe' b@ takes /O(1)/.
-- Map key\/value pairs and collect the 'Just' results.
--
-- > mapMaybeWithKey (\k a -> if k > 1 && a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
-- >   === singleton 2 "new a"
mapMaybeWithKey :: (k -> a -> Maybe b) -> Multimap k a -> Multimap k b
mapMaybeWithKey f (Multimap (m, _)) = fromMap $
  Map.mapMaybeWithKey (\k -> nonEmpty . Maybe.mapMaybe (f k) . Nel.toList) m

-- | /O(n)/, assuming the function @a -> 'Either' b c@ takes /O(1)/.
-- Map values and separate the 'Left' and 'Right' results.
--
-- > mapEither (\a -> if a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
-- >   === (fromList [(1,'a'),(2,'a')],fromList [(1,'c'),(2,'c')])
mapEither :: (a -> Either b c) -> Multimap k a -> (Multimap k b, Multimap k c)
mapEither = mapEitherWithKey . const

-- | /O(n)/, assuming the function @k -> a -> 'Either' b c@ takes /O(1)/.
-- Map key\/value pairs and separate the 'Left' and 'Right' results.
--
-- > mapEitherWithKey (\k a -> if even k && a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
-- >   === (fromList [(2,'a')],fromList [(1,'a'),(1,'c'),(2,'c')])
mapEitherWithKey :: (k -> a -> Either b c) -> Multimap k a -> (Multimap k b, Multimap k c)
mapEitherWithKey f (Multimap (m, _)) =
    (fromMap' . Map.mapWithKey (const fst) &&& fromMap' . Map.mapWithKey (const snd))
      $ Map.mapWithKey g m
  where
    g k as = Either.partitionEithers $ fmap (f k) (Nel.toList as)

------------------------------------------------------------------------------

-- | /O(log n)/. Return the smallest key and the associated values. Returns 'Nothing'
-- if the map is empty.
--
-- > lookupMin (fromList [(1,'a'),(1,'c'),(2,'c')]) === Just (1, NonEmpty.fromList "ac")
-- > lookupMin (empty :: Multimap Int Char) === Nothing
lookupMin :: Multimap k a -> Maybe (k, NonEmpty a)
lookupMin (Multimap (m, _)) = Map.lookupMin m

-- | /O(log n)/. Return the largest key and the associated values. Returns 'Nothing'
-- if the map is empty.
--
-- > lookupMax (fromList [(1,'a'),(1,'c'),(2,'c')]) === Just (2, NonEmpty.fromList "c")
-- > lookupMax (empty :: Multimap Int Char) === Nothing
lookupMax :: Multimap k a -> Maybe (k, NonEmpty a)
lookupMax (Multimap (m, _)) = Map.lookupMax m

-- | /O(log n)/. Return the largest key smaller than the given one, and the associated
-- values, if exist.
--
-- > lookupLT 1 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Nothing
-- > lookupLT 4 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (3, NonEmpty.fromList "bc")
lookupLT :: Ord k => k -> Multimap k a -> Maybe (k, NonEmpty a)
lookupLT k (Multimap (m, _)) = Map.lookupLT k m

-- | /O(log n)/. Return the smallest key larger than the given one, and the associated
-- values, if exist.
--
-- > lookupGT 5 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Nothing
-- > lookupGT 2 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (3, NonEmpty.fromList "bc")
lookupGT :: Ord k => k -> Multimap k a -> Maybe (k, NonEmpty a)
lookupGT k (Multimap (m, _)) = Map.lookupGT k m

-- | /O(log n)/. Return the largest key smaller than or equal to the given one, and the associated
-- values, if exist.
--
-- > lookupLE 0 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Nothing
-- > lookupLE 1 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (1, NonEmpty.fromList "a")
-- > lookupLE 4 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (3, NonEmpty.fromList "bc")
lookupLE :: Ord k => k -> Multimap k a -> Maybe (k, NonEmpty a)
lookupLE k (Multimap (m, _)) = Map.lookupLE k m

-- | /O(log n)/. Return the smallest key larger than or equal to the given one, and the associated
-- values, if exist.
--
-- > lookupGE 6 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Nothing
-- > lookupGE 5 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (5, NonEmpty.fromList "c")
-- > lookupGE 2 (fromList [(1,'a'),(3,'b'),(3,'c'),(5,'c')]) === Just (3, NonEmpty.fromList "bc")
lookupGE :: Ord k => k -> Multimap k a -> Maybe (k, NonEmpty a)
lookupGE k (Multimap (m, _)) = Map.lookupGE k m
