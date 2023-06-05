{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap.Table.Internal
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
module Data.Multimap.Table.Internal (
  Table (..)
  , Size

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

import           Control.Arrow ((&&&))
import           Data.Data (Data)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)

import Prelude hiding (Foldable(..), filter, lookup, map)

infixl 9 !,!?

type Size = Int

newtype Table r c a = Table (Map r (Map c a), Map c (Map r a), Size)
  deriving (Eq, Ord, Data)

instance (Show r, Show c, Show a) => Show (Table r c a) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance (Ord r, Ord c, Read r, Read c, Read a) => Read (Table r c a) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    pure (fromList xs,t)

instance Functor (Table r c) where
  fmap = map

instance Foldable.Foldable (Table r c) where
  foldMap = foldMapWithKeys . const . const
  {-# INLINE foldMap #-}

instance (Ord r, Ord c) => Traversable (Table r c) where
  traverse = traverseWithKeys . const . const
  {-# INLINE traverse #-}

instance (Ord r, Ord c) => Semigroup (Table r c a) where
  (<>) = union

instance (Ord r, Ord c) => Monoid (Table r c a) where
  mempty = empty
  mappend = (<>)

------------------------------------------------------------------------------

-- | /O(1)/. The empty table.
--
-- > size empty === 0
empty :: Table r c a
empty = Table (Map.empty, Map.empty, 0)

-- | /O(1)/. A table with a single element.
--
-- > singleton 1 'a' "a" === fromList [(1,'a',"a")]
-- > size (singleton 1 'a' "a") === 1
singleton :: r -> c -> a -> Table r c a
singleton r c a = Table (Map.singleton r (Map.singleton c a), Map.singleton c (Map.singleton r a), 1)

-- | Build a table from a list of key\/value pairs.
--
-- > fromList ([] :: [(Int, Char, String)]) === empty
fromList :: (Ord r, Ord c) => [(r, c, a)] -> Table r c a
fromList = Foldable.foldr (uncurry3 insert) empty

-- | Build a table from a row map.
--
-- > fromRowMap (Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]), (2, Map.fromList [('a',"d")])])
-- >   === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
fromRowMap :: (Ord r, Ord c) => Map r (Map c a) -> Table r c a
fromRowMap m = Table (m', transpose' m', size' m')
  where m' = nonEmpty m

-- | Build a table from a column map.
--
-- > fromColumnMap (Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]), (2, Map.fromList [('a',"d")])])
-- >   === fromList [('a',1,"b"),('a',2,"d"),('b',1,"c")]
fromColumnMap :: (Ord r, Ord c) => Map c (Map r a) -> Table r c a
fromColumnMap m = Table (transpose' m', m', size' m')
  where m' = nonEmpty m

-- | Flip the row and column keys.
--
-- > transpose (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [('a',1,"b"),('a',2,"d"),('b',1,"c")]
transpose :: Table r c a -> Table c r a
transpose (Table (rm, cm, sz)) = Table (cm, rm, sz)

------------------------------------------------------------------------------

-- | /O(log k)/. Associate with value with the row key and the column key.
-- If the table already contains a value for those keys, the value is replaced.
--
-- > insert 1 'a' "a" empty === singleton 1 'a' "a"
-- > insert 1 'a' "a" (fromList [(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"d")]
-- > insert 1 'a' "a" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"d")]
insert :: (Ord r, Ord c) => r -> c -> a -> Table r c a -> Table r c a
insert r c a (Table (rm, cm, _)) = fromMaps' r c rm' cm'
  where
    rm' = Map.alter f r rm
    cm' = Map.alter g c cm
    f = Just . maybe (Map.singleton c a) (Map.insert c a)
    g = Just . maybe (Map.singleton r a) (Map.insert r a)

-- | /O(log k)/. Remove the value associated with the given keys.
--
-- > delete 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'b',"c"),(2,'a',"d")]
-- > delete 1 'a' (fromList [(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'b',"c"),(2,'a',"d")]
delete :: (Ord r, Ord c) => r -> c -> Table r c a -> Table r c a
delete r c (Table (rm, cm, _)) = fromMaps' r c rm' cm'
  where
    rm' = Map.adjust (Map.delete c) r rm
    cm' = Map.adjust (Map.delete r) c cm

-- | Remove an entire row.
--
-- > deleteRow 1 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
-- > deleteRow 3 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
deleteRow :: Ord r => r -> Table r c a -> Table r c a
deleteRow r (Table (rm, cm, _)) = Table (rm', cm', size' rm')
  where
    rm' = Map.delete r rm
    cm' = nonEmpty $ Map.map (Map.delete r) cm

-- | Remove an entire column.
--
-- > deleteColumn 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
-- > deleteColumn 'z' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
deleteColumn :: Ord c => c -> Table r c a -> Table r c a
deleteColumn c (Table (rm, cm, _)) = Table (rm', cm', size' cm')
  where
    rm' = nonEmpty $ Map.map (Map.delete c) rm
    cm' = Map.delete c cm

-- | /O(log k)/, assuming the function @a -> a@ takes /O(1)/.
-- Update the value at a specific row key and column key, if exists.
--
-- > adjust ("new " ++) 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"new b"),(1,'b',"c"),(2,'a',"d")]
adjust :: (Ord r, Ord c) => (a -> a) -> r -> c -> Table r c a -> Table r c a
adjust = adjustWithKeys . const . const

-- | /O(log k)/, assuming the function @r -> c -> a -> a@ takes /O(1)/.
-- Update the value at a specific row key and column key, if exists.
--
-- > adjustWithKeys (\r c x -> show r ++ ":" ++ show c ++ ":new " ++ x) 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
-- >   === fromList [(1,'a',"1:'a':new b"),(1,'b',"c"),(2,'a',"d")]
adjustWithKeys
  :: (Ord r, Ord c)
  => (r -> c -> a -> a) -> r -> c -> Table r c a -> Table r c a
adjustWithKeys f = updateWithKeys (\r c a -> Just (f r c a))

-- | /O(log k)/, assuming the function @a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'update' f r c table@) updates the value at the given
-- row and column keys, if exists. If @f@ returns 'Nothing', the value
-- associated with those keys, if exists is deleted.
--
-- > let f x = if x == "b" then Just "new b" else Nothing in do
-- >   update f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"new b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   update f 1 'a' (fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
update :: (Ord r, Ord c) => (a -> Maybe a) -> r -> c -> Table r c a -> Table r c a
update = updateWithKeys . const . const

-- | /O(log k)/, assuming the function @r -> c -> a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'updateWithKeys' f r c table@) updates the value at the given
-- row and column keys, if exists. If @f@ returns 'Nothing', the value
-- associated with those keys, if exists is deleted.
--
-- > let f r c x = if x == "b" then Just (show r ++ ":" ++ show c ++ ":new b") else Nothing in do
-- >   updateWithKeys f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"1:'a':new b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   updateWithKeys f 1 'a' (fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
updateWithKeys
  :: (Ord r, Ord c)
  => (r -> c -> a -> Maybe a) -> r -> c -> Table r c a -> Table r c a
updateWithKeys f = alterWithKeys (\r c -> (>>= f r c))

-- | /O(log k)/, assuming the function @'Maybe' a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'alter' f r c table@) alters the value at the given
-- row and column keys, if exists. It can be used to insert, delete
-- or update a value.
--
-- > let (f,g,h) = (const Nothing, const (Just "hello"), fmap ('z':)) in do
-- >   alter f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alter f 4 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alter f 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alter g 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"hello"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alter g 4 'e' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c"),(4,'e',"hello")]
-- >   alter h 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"zb"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alter h 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
alter :: (Ord r, Ord c) => (Maybe a -> Maybe a) -> r -> c -> Table r c a -> Table r c a
alter = alterWithKeys . const . const

-- | /O(log k)/, assuming the function @r -> c -> 'Maybe' a -> 'Maybe' a@ takes /O(1)/.
-- The expression (@'alterWithKeys' f r c table@) alters the value at the given
-- row and column keys, if exists. It can be used to insert, delete
-- or update a value.
--
-- > let (f,g) = (\_ _ _ -> Nothing, \r c -> fmap ((show r ++ ":" ++ show c ++ ":") ++)) in do
-- >   alterWithKeys f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alterWithKeys f 4 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alterWithKeys f 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alterWithKeys g 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"1:'a':b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
-- >   alterWithKeys g 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
alterWithKeys
  :: (Ord r, Ord c)
  => (r -> c -> Maybe a -> Maybe a) -> r -> c -> Table r c a -> Table r c a
alterWithKeys f r c tbl@(Table (rm, cm, _))
  | Just a <- f r c (lookup r c tbl) =
      let rm' = Map.alter (Just . maybe (Map.singleton c a) (Map.insert c a)) r rm
          cm' = Map.alter (Just . maybe (Map.singleton r a) (Map.insert r a)) c cm
       in fromMaps' r c rm' cm'
  | otherwise = delete r c tbl

------------------------------------------------------------------------------

-- | /O(log k)/. Lookup the values at a row key and column key in the map.
lookup :: (Ord r, Ord c) => r -> c -> Table r c a -> Maybe a
lookup r c (Table (rm, _, _)) = Map.lookup r rm >>= Map.lookup c

-- | /O(log k)/. Lookup the values at a row key and column key in the map.
--
-- > fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] !? (1,'a') === Just "b"
-- > fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] !? (1,'c') === Nothing
(!?) :: (Ord r, Ord c) => Table r c a -> (r, c) -> Maybe a
(!?) = flip (uncurry lookup)

-- | /O(log k)/. Lookup the values at a row key and column key in the map.
-- Calls 'error' if the value does not exist.
--
-- > fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] ! (1,'a') === "b"
(!) :: (Ord r, Ord c) => Table r c a -> (r, c) -> a
(!) tbl keys =
  Maybe.fromMaybe (error "Table.!: cell does not exist") (tbl !? keys)

-- | /O(log k)/. Is there a value associated with the given row and
-- column keys?
--
-- > hasCell (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (1,'a') === True
-- > hasCell (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (1,'c') === False
hasCell :: (Ord r, Ord c) => Table r c a -> (r, c) -> Bool
hasCell (Table (rm, _, _)) (r, c) =
  maybe False (Map.member c) (Map.lookup r rm)

-- | /O(log r)/. Is there a row with the given row key that has at least
--  one value?
--
-- > hasRow (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 1 === True
-- > hasRow (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 3 === False
hasRow :: Ord r => Table r c a -> r -> Bool
hasRow (Table (rm, _, _)) r = Map.member r rm

-- | /O(log c)/. Is there a column with the given column key that has at least
-- one value?
--
-- > hasColumn (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 'a' === True
-- > hasColumn (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 'c' === False
hasColumn :: Ord c => Table r c a -> c -> Bool
hasColumn (Table (_, cm, _)) c = Map.member c cm

-- | /O(1)/. Is the table empty?
--
-- > Data.Multimap.Table.null empty === True
-- > Data.Multimap.Table.null (singleton 1 'a' "a") === False
null :: Table r c a -> Bool
null (Table (rm, _, _)) = Map.null rm

-- | /O(1)/. Is the table non-empty?
--
-- > notNull empty === False
-- > notNull (singleton 1 'a' "a") === True
notNull :: Table r c a -> Bool
notNull = not . null

-- | The total number of values for all row and column keys.
--
-- @size@ is evaluated lazily. Forcing the size for the first time takes up to
-- /O(n)/ and subsequent forces take /O(1)/.
--
-- > size empty === 0
-- > size (singleton 1 'a' "a") === 1
-- > size (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === 3
size :: Table r c a -> Int
size (Table (_, _, sz)) = sz

------------------------------------------------------------------------------

-- | Union two tables, preferring values from the first table
-- upon duplicate keys.
--
-- > union (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
-- >   === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
union :: (Ord r, Ord c) => Table r c a -> Table r c a -> Table r c a
union = unionWith const

-- | Union a number of tables, preferring values from the leftmost table
-- upon duplicate keys.
--
-- > unions [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
-- >   === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
unions :: (Foldable f, Ord r, Ord c) => f (Table r c a) -> Table r c a
unions = Foldable.foldr union empty

-- | Union two tables with a combining function for duplicate keys.
--
-- > unionWith (++) (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
-- >   === fromList [(1,'a',"bc"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
unionWith :: (Ord r, Ord c) => (a -> a -> a) -> Table r c a -> Table r c a -> Table r c a
unionWith = unionWithKeys . const . const

-- | Union two tables with a combining function for duplicate keys.
--
-- > let f r c a a' = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ a' in do
-- >   unionWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
-- >     === fromList [(1,'a',"1:'a':b|c"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
unionWithKeys
  :: (Ord r, Ord c)
  => (r -> c -> a -> a -> a)
  -> Table r c a -> Table r c a -> Table r c a
unionWithKeys f (Table (rm1, cm1, _)) (Table (rm2, cm2, _)) = fromMaps rm cm
  where
    rm = Map.unionWithKey (Map.unionWithKey . f) rm1 rm2
    cm = Map.unionWithKey (Map.unionWithKey . flip f) cm1 cm2

-- | Union a number of tables with a combining function for duplicate keys.
--
-- > unionsWith (++) [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
-- >   === fromList [(1,'a',"bc"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
unionsWith :: (Foldable f, Ord r, Ord c) => (a -> a -> a) -> f (Table r c a) -> Table r c a
unionsWith f = Foldable.foldr (unionWith f) empty

-- | Union a number of tables with a combining function for duplicate keys.
--
-- > let f r c a a' = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ a' in do
-- >   unionsWithKeys f [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
-- >     === fromList [(1,'a',"1:'a':b|c"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
unionsWithKeys
  :: (Foldable f, Ord r, Ord c)
  => (r -> c -> a -> a -> a)
  -> f (Table r c a) -> Table r c a
unionsWithKeys f = Foldable.foldr (unionWithKeys f) empty

-- | Difference of two tables. Return values in the first table whose
-- row and column keys do not have an associated value in the second table.
--
-- > difference (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(1,'b',"d"),(2,'b',"b")])
-- >   === singleton 2 'a' "b"
difference :: (Ord r, Ord c) => Table r c a -> Table r c a -> Table r c a
difference (Table (rm1, cm1, _)) (Table (rm2, cm2, _)) = fromMaps rm cm
  where
    rm = Map.differenceWith ((Just .) . Map.difference) rm1 rm2
    cm = Map.differenceWith ((Just .) . Map.difference) cm1 cm2

------------------------------------------------------------------------------

-- | /O(n)/, assuming the function @a -> b@ takes /O(1)/.
-- Map a function over all values in the table.
--
-- > Data.Multimap.Table.map (++ "x") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === fromList [(1,'a',"bx"),(1,'b',"cx"),(2,'a',"bx")]
map :: (a -> b) -> Table r c a -> Table r c b
map = mapWithKeys . const . const

-- | /O(n)/, assuming the function @r -> c -> a -> b@ takes /O(1)/.
-- Map a function over all values in the table.
--
-- > mapWithKeys (\r c x -> show r ++ ":" ++ show c ++ ":" ++ x) (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")])
-- >   === fromList [(1,'a',"1:'a':b"),(1,'b',"1:'b':c"),(2,'a',"2:'a':b")]
mapWithKeys :: (r -> c -> a -> b) -> Table r c a -> Table r c b
mapWithKeys f (Table (rm, cm, sz)) = Table (rm', cm', sz)
  where
    rm' = Map.mapWithKey (Map.mapWithKey . f) rm
    cm' = Map.mapWithKey (Map.mapWithKey . flip f) cm

-- | Traverse the (row key, column key, value) triples and collect the results.
--
-- > let f r c a = if odd r && c > 'a' then Just (a ++ "x") else Nothing in do
-- >   traverseWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === Nothing
-- >   traverseWithKeys f (fromList [(1,'b',"b"),(1,'c',"c"),(3,'d',"b")]) === Just (fromList [(1,'b',"bx"),(1,'c',"cx"),(3,'d',"bx")])
traverseWithKeys
  :: (Applicative t, Ord r, Ord c)
  => (r -> c -> a -> t b)
  -> Table r c a
  -> t (Table r c b)
traverseWithKeys f (Table (rm, _, _)) = fromMaps <$> rm' <*> cm'
  where
    rm' = Map.traverseWithKey (Map.traverseWithKey . f) rm
    cm' = transpose' <$> rm'

-- | Traverse the (row key, column key, value) triples and collect the 'Just' results.
traverseMaybeWithKeys
  :: (Applicative t, Ord r, Ord c)
  => (r -> c -> a -> t (Maybe b))
  -> Table r c a
  -> t (Table r c b)
traverseMaybeWithKeys f (Table (rm, _, _)) = fromMaps <$> rm' <*> cm'
  where
    rm' = Map.traverseWithKey (Map.traverseMaybeWithKey . f) rm
    cm' = transpose' <$> rm'

------------------------------------------------------------------------------

-- | /O(n)/. Fold the values in the table row by row using the given
-- right-associative binary operator.
--
-- > Data.Multimap.Table.foldr (:) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "bcd"
foldr :: (a -> b -> b) -> b -> Table r c a -> b
foldr = foldrWithKeys . const . const

-- | /O(n)/. Fold the values in the table row by row using the given
-- left-associative binary operator.
--
-- > Data.Multimap.Table.foldl (flip (:)) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "dcb"
foldl :: (a -> b -> a) -> a -> Table r c b -> a
foldl f = foldlWithKeys (\a _ _ -> f a)

-- | /O(n)/. Fold the (row key, column key value) triplets in the table
--  row by row using the given right-associative binary operator.
--
-- > let f r c a b = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ b in do
-- >   foldrWithKeys f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
foldrWithKeys :: (r -> c -> a -> b -> b) -> b -> Table r c a -> b
foldrWithKeys f b (Table (rm, _, _)) = Map.foldrWithKey f' b rm
  where
    f' = flip . Map.foldrWithKey . f

-- | /O(n)/. Fold the (row key, column key, value) triplets in the table
--  row by row using the given left-associative binary operator.
--
-- > let f a r c b = show r ++ ":" ++ show c ++ ":" ++ b ++ "|" ++ a in do
-- >   foldlWithKeys f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "2:'a':d|1:'b':c|1:'a':b|"
foldlWithKeys :: (a -> r -> c -> b -> a) -> a -> Table r c b -> a
foldlWithKeys f a (Table (rm, _, _)) = Map.foldlWithKey f' a rm
  where
    f' = flip (Map.foldlWithKey . flip f)

-- | /O(n)/. A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.Table.foldr' (:) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "bcd"
foldr' :: (a -> b -> b) -> b -> Table r c a -> b
foldr' = foldrWithKeys' . const . const

-- | /O(n)/. A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > Data.Multimap.Table.foldl' (flip (:)) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "dcb"
foldl' :: (a -> b -> a) -> a -> Table r c b -> a
foldl' f = foldlWithKeys' (\a _ _ -> f a)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > let f r c a b = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ b in do
-- >   foldrWithKeys' f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
foldrWithKeys' :: (r -> c -> a -> b -> b) -> b -> Table r c a -> b
foldrWithKeys' f b (Table (rm, _, _)) = Map.foldrWithKey' f' b rm
  where
    f' = flip . Map.foldrWithKey' . f

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
--
-- > let f a r c b = show r ++ ":" ++ show c ++ ":" ++ b ++ "|" ++ a in do
-- >   foldlWithKeys' f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "2:'a':d|1:'b':c|1:'a':b|"
foldlWithKeys' :: (a -> r -> c -> b -> a) -> a -> Table r c b -> a
foldlWithKeys' f a (Table (rm, _, _)) = Map.foldlWithKey' f' a rm
  where
    f' = flip (Map.foldlWithKey' . flip f)

-- | /O(n)/. Fold the (row key, column key, value) triplets in the map
-- row by row using the given monoid.
--
-- > let f r c a = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" in do
-- >   foldMapWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
foldMapWithKeys :: Monoid m => (r -> c -> a -> m) -> Table r c a -> m
foldMapWithKeys f (Table (rm, _, _)) = Map.foldMapWithKey f' rm
  where
    f' = Map.foldMapWithKey . f

------------------------------------------------------------------------------

-- | /O(r)/. Return a mapping from column keys to values for the given
-- row key.
--
-- > row 1 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.fromList [('a',"b"),('b',"c")]
-- > row 3 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.empty
row :: Ord r => r -> Table r c a -> Map c a
row r (Table (rm, _, _)) = Map.findWithDefault Map.empty r rm

-- | /O(c)/. Return a mapping from row keys to values for the given
-- column key.
--
-- > column 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.fromList [(1,"b"),(2,"d")]
-- > column 'c' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.empty
column :: Ord c => c -> Table r c a -> Map r a
column c (Table (_, cm, _)) = Map.findWithDefault Map.empty c cm

-- | Return a mapping from row keys to maps from column keys to values.
--
-- > rowMap (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
-- >   === Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]),(2, Map.fromList [('a',"d")])]
rowMap :: Table r c a -> Map r (Map c a)
rowMap (Table (rm, _, _)) = rm

-- | Return a mapping from column keys to maps from row keys to values.
--
-- > columnMap (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
-- >   === Map.fromList [('a', Map.fromList [(1,"b"),(2,"d")]),('b', Map.fromList [(1,"c")])]
columnMap :: Table r c a -> Map c (Map r a)
columnMap (Table (_, cm, _)) = cm

-- | Return, in ascending order, the list of all row keys of that have
-- at least one value in the table.
--
-- > rowKeys (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [1,2]
rowKeys :: Table r c a -> [r]
rowKeys (Table (rm, _, _)) = Map.keys rm

-- | Return, in ascending order, the list of all column keys of that have
-- at least one value in the table.
--
-- > columnKeys (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === ['a','b']
columnKeys :: Table r c a -> [c]
columnKeys (Table (_, cm, _)) = Map.keys cm

-- | Return the set of all row keys of that have at least one value
-- in the table.
--
-- > rowKeysSet (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Set.fromList [1,2]
rowKeysSet :: Table r c a -> Set r
rowKeysSet (Table (rm, _, _)) = Map.keysSet rm

-- | Return the set of all column keys of that have at least one value
-- in the table.
--
-- > columnKeysSet (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Set.fromList ['a','b']
columnKeysSet :: Table r c a -> Set c
columnKeysSet (Table (_, cm, _)) = Map.keysSet cm

-- | Convert the table into a list of (row key, column key, value) triples.
--
-- > toList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
toList :: Table r c a -> [(r, c, a)]
toList (Table (rm, _, _)) = Map.toList (Map.toList <$> rm) >>= distr

-- | Convert the table into a list of (row key, column key, value) triples
-- in ascending order of row keys, and ascending order of column keys
-- with a row.
--
-- > toRowAscList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
toRowAscList :: Table r c a -> [(r, c, a)]
toRowAscList (Table (rm, _, _)) = Map.toAscList (Map.toAscList <$> rm) >>= distr

-- | Convert the table into a list of (column key, row key, value) triples
-- in ascending order of column keys, and ascending order of row keys
-- with a column.
--
-- > toColumnAscList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [('a',1,"b"),('a',2,"d"),('b',1,"c")]
toColumnAscList :: Table r c a -> [(c, r, a)]
toColumnAscList (Table (_, cm, _)) = Map.toAscList (Map.toAscList <$> cm) >>= distr

-- | Convert the table into a list of (row key, column key, value) triples
-- in descending order of row keys, and descending order of column keys
-- with a row.
--
-- > toRowDescList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(2,'a',"d"),(1,'b',"c"),(1,'a',"b")]
toRowDescList :: Table r c a -> [(r, c, a)]
toRowDescList (Table (rm, _, _)) = Map.toDescList (Map.toDescList <$> rm) >>= distr

-- | Convert the table into a list of (column key, row key, value) triples
-- in descending order of column keys, and descending order of row keys
-- with a column.
--
-- > toColumnDescList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [('b',1,"c"),('a',2,"d"),('a',1,"b")]
toColumnDescList :: Table r c a -> [(c, r, a)]
toColumnDescList (Table (_, cm, _)) = Map.toDescList (Map.toDescList <$> cm) >>= distr

------------------------------------------------------------------------------

-- | /O(n)/, assuming the predicate function takes /O(1)/.
-- Retain all values that satisfy the predicate.
--
-- > Data.Multimap.Table.filter (> "c") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
-- > Data.Multimap.Table.filter (> "d") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === empty
filter :: (a -> Bool) -> Table r c a -> Table r c a
filter = filterWithKeys . const . const

-- | /O(r)/, assuming the predicate function takes /O(1)/.
-- Retain all rows that satisfy the predicate.
--
-- > filterRow even (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
filterRow :: (r -> Bool) -> Table r c a -> Table r c a
filterRow p (Table (rm, cm, _)) = Table (rm', nonEmpty cm', size' rm')
  where
    rm' = Map.filterWithKey (const . p) rm
    cm' = Map.map (Map.filterWithKey (const . p)) cm

-- | /O(c)/, assuming the predicate function takes /O(1)/.
-- Retain all columns that satisfy the predicate.
--
-- > filterColumn (> 'a') (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
filterColumn :: (c -> Bool) -> Table r c a -> Table r c a
filterColumn p (Table (rm, cm, _)) = Table (nonEmpty rm', cm', size' cm')
  where
    rm' = Map.map (Map.filterWithKey (const . p)) rm
    cm' = Map.filterWithKey (const . p) cm

-- | /O(c)/, assuming the predicate function takes /O(1)/.
-- Retain all (row key, column key, value) triples that satisfy the predicate.
--
-- > filterWithKeys (\r c a -> odd r && c > 'a' && a > "b") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
filterWithKeys
  :: (r -> c -> a -> Bool)
  -> Table r c a
  -> Table r c a
filterWithKeys p (Table (rm, cm, _)) = fromMaps rm' cm'
  where
    rm' = Map.mapWithKey (Map.filterWithKey . p) rm
    cm' = Map.mapWithKey (Map.filterWithKey . flip p) cm

-- | /O(n)/, assuming the function @a -> 'Maybe' b@ takes /O(1)/.
-- Map values and collect the 'Just' results.
--
-- > mapMaybe (\a -> if a == "a" then Just "new a" else Nothing) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
-- >   === fromList [(1,'a',"new a"),(2,'b',"new a")]
mapMaybe :: (a -> Maybe b) -> Table r c a -> Table r c b
mapMaybe = mapMaybeWithKeys . const . const

-- | /O(n)/, assuming the function @r -> c -> a -> 'Maybe' b@ takes /O(1)/.
-- Map (row key, column key, value) triples and collect the 'Just' results.
--
-- > let f r c a = if r == 1 && a == "c" then Just "new c" else Nothing in do
-- >   mapMaybeWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "new c"
mapMaybeWithKeys :: (r -> c -> a -> Maybe b) -> Table r c a -> Table r c b
mapMaybeWithKeys f (Table (rm, cm, _)) = fromMaps rm' cm'
  where
    rm' = Map.mapWithKey (Map.mapMaybeWithKey . f) rm
    cm' = Map.mapWithKey (Map.mapMaybeWithKey . flip f) cm

-- | /O(n)/, assuming the function @a -> 'Either' a1 a2@ takes /O(1)/.
-- Map values and separate the 'Left' and 'Right' results.
--
-- > mapEither (\a -> if a == "a" then Left a else Right a) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
-- >   === (fromList [(1,'a',"a"),(2,'b',"a")],fromList [(1,'b',"c")])
mapEither :: (a -> Either a1 a2) -> Table r c a -> (Table r c a1, Table r c a2)
mapEither = mapEitherWithKeys . const . const

-- | /O(n)/, assuming the function @r -> c -> a -> 'Either' a1 a2@ takes /O(1)/.
-- Map (row key, column key, value) triples and separate the 'Left' and 'Right' results.
--
-- > mapEitherWithKeys (\r c a -> if r == 1 && c == 'a' then Left a else Right a) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
-- >   === (fromList [(1,'a',"a")],fromList [(1,'b',"c"),(2,'b',"a")])
mapEitherWithKeys :: (r -> c -> a -> Either a1 a2) -> Table r c a -> (Table r c a1, Table r c a2)
mapEitherWithKeys f (Table (rm, cm, _)) = (fromMaps rm1 cm1, fromMaps rm2 cm2)
  where
    (rm1, rm2) = (fmap fst &&& fmap snd) $ Map.mapWithKey (Map.mapEitherWithKey . f) rm
    (cm1, cm2) = (fmap fst &&& fmap snd) $ Map.mapWithKey (Map.mapEitherWithKey . flip f) cm

------------------------------------------------------------------------------
-- * Non exported functions
------------------------------------------------------------------------------

assoc :: (a, (b, c)) -> (a, b, c)
assoc (a, (b, c)) = (a, b, c)

distr :: (a, [(b, c)]) -> [(a, b, c)]
distr = fmap assoc . uncurry (zip . repeat)

-- | Build a table from a row map and a column map.
fromMaps :: Map r (Map c a) -> Map c (Map r a) -> Table r c a
fromMaps rm cm = Table (rm', cm', size' rm')
  where
    rm' = nonEmpty rm
    cm' = nonEmpty cm

fromMaps' :: (Ord r, Ord c) => r -> c -> Map r (Map c a) -> Map c (Map r a) -> Table r c a
fromMaps' r c rm cm = Table (rm', cm', size' rm')
  where
    rm' = nonEmpty' r rm
    cm' = nonEmpty' c cm

nonEmpty :: Map k1 (Map k2 a) -> Map k1 (Map k2 a)
nonEmpty = Map.filter (not . Map.null)

nonEmpty' :: Ord k1 => k1 -> Map k1 (Map k2 a) -> Map k1 (Map k2 a)
nonEmpty' k1 m = case Map.lookup k1 m of
  Just m' | Map.null m' -> Map.delete k1 m
  _ -> m

transpose' :: (Ord r, Ord c) => Map r (Map c a) -> Map c (Map r a)
transpose' = Map.foldrWithKey' f Map.empty
  where
    f r = Map.unionWith Map.union . Map.map (Map.singleton r)

size' :: Map k1 (Map k2 a) -> Int
size' = Foldable.sum . fmap Map.size

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c) = f a b c
