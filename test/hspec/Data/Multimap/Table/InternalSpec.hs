-- Generated code, do not modify by hand. Generate by running TestGen.hs.

{-# OPTIONS_GHC -w #-}
module Data.Multimap.Table.InternalSpec where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Multimap.Table.Internal as Data.Multimap.Table

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.Multimap.Table.Internal" $ do
    it "" $ do
      size empty === 0
      singleton 1 'a' "a" === fromList [(1,'a',"a")]
      size (singleton 1 'a' "a") === 1
      fromList ([] :: [(Int, Char, String)]) === empty
      fromRowMap (Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]), (2, Map.fromList [('a',"d")])])
        === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
      fromColumnMap (Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]), (2, Map.fromList [('a',"d")])])
        === fromList [('a',1,"b"),('a',2,"d"),('b',1,"c")]
      transpose (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [('a',1,"b"),('a',2,"d"),('b',1,"c")]
      insert 1 'a' "a" empty === singleton 1 'a' "a"
      insert 1 'a' "a" (fromList [(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"d")]
      insert 1 'a' "a" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"d")]
      delete 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'b',"c"),(2,'a',"d")]
      delete 1 'a' (fromList [(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'b',"c"),(2,'a',"d")]
      deleteRow 1 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
      deleteRow 3 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
      deleteColumn 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
      deleteColumn 'z' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
      adjust ("new " ++) 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === fromList [(1,'a',"new b"),(1,'b',"c"),(2,'a',"d")]
      adjustWithKeys (\r c x -> show r ++ ":" ++ show c ++ ":new " ++ x) 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
        === fromList [(1,'a',"1:'a':new b"),(1,'b',"c"),(2,'a',"d")]
      let f x = if x == "b" then Just "new b" else Nothing in do
        update f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"new b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        update f 1 'a' (fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
      let f r c x = if x == "b" then Just (show r ++ ":" ++ show c ++ ":new b") else Nothing in do
        updateWithKeys f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"1:'a':new b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        updateWithKeys f 1 'a' (fromList [(1,'a',"a"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
      let (f,g,h) = (const Nothing, const (Just "hello"), fmap ('z':)) in do
        alter f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alter f 4 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alter f 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alter g 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"hello"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alter g 4 'e' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c"),(4,'e',"hello")]
        alter h 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"zb"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alter h 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
      let (f,g) = (\_ _ _ -> Nothing, \r c -> fmap ((show r ++ ":" ++ show c ++ ":") ++)) in do
        alterWithKeys f 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alterWithKeys f 4 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alterWithKeys f 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alterWithKeys g 1 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"1:'a':b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
        alterWithKeys g 2 'b' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]) === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(3,'a',"c")]
      fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] !? (1,'a') === Just "b"
      fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] !? (1,'c') === Nothing
      fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")] ! (1,'a') === "b"
      hasCell (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (1,'a') === True
      hasCell (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (1,'c') === False
      hasRow (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 1 === True
      hasRow (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 3 === False
      hasColumn (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 'a' === True
      hasColumn (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) 'c' === False
      Data.Multimap.Table.null empty === True
      Data.Multimap.Table.null (singleton 1 'a' "a") === False
      notNull empty === False
      notNull (singleton 1 'a' "a") === True
      size empty === 0
      size (singleton 1 'a' "a") === 1
      size (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === 3
      union (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
        === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      unions [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
        === fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      unionWith (++) (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
        === fromList [(1,'a',"bc"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      let f r c a a' = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ a' in do
        unionWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")])
          === fromList [(1,'a',"1:'a':b|c"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      unionsWith (++) [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
        === fromList [(1,'a',"bc"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      let f r c a a' = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ a' in do
        unionsWithKeys f [fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")], fromList [(1,'a',"c"),(2,'b',"d"),(3,'c',"e")]]
          === fromList [(1,'a',"1:'a':b|c"),(1,'b',"c"),(2,'a',"b"),(2,'b',"d"),(3,'c',"e")]
      difference (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) (fromList [(1,'a',"c"),(1,'b',"d"),(2,'b',"b")])
        === singleton 2 'a' "b"
      Data.Multimap.Table.map (++ "x") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === fromList [(1,'a',"bx"),(1,'b',"cx"),(2,'a',"bx")]
      mapWithKeys (\r c x -> show r ++ ":" ++ show c ++ ":" ++ x) (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")])
        === fromList [(1,'a',"1:'a':b"),(1,'b',"1:'b':c"),(2,'a',"2:'a':b")]
      let f r c a = if odd r && c > 'a' then Just (a ++ "x") else Nothing in do
        traverseWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"b")]) === Nothing
        traverseWithKeys f (fromList [(1,'b',"b"),(1,'c',"c"),(3,'d',"b")]) === Just (fromList [(1,'b',"bx"),(1,'c',"cx"),(3,'d',"bx")])
      Data.Multimap.Table.foldr (:) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "bcd"
      Data.Multimap.Table.foldl (flip (:)) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "dcb"
      let f r c a b = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ b in do
        foldrWithKeys f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
      let f a r c b = show r ++ ":" ++ show c ++ ":" ++ b ++ "|" ++ a in do
        foldlWithKeys f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "2:'a':d|1:'b':c|1:'a':b|"
      Data.Multimap.Table.foldr' (:) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "bcd"
      Data.Multimap.Table.foldl' (flip (:)) "" (fromList [(1,'a','b'),(1,'b','c'),(2,'a','d')]) === "dcb"
      let f r c a b = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" ++ b in do
        foldrWithKeys' f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
      let f a r c b = show r ++ ":" ++ show c ++ ":" ++ b ++ "|" ++ a in do
        foldlWithKeys' f "" (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "2:'a':d|1:'b':c|1:'a':b|"
      let f r c a = show r ++ ":" ++ show c ++ ":" ++ a ++ "|" in do
        foldMapWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === "1:'a':b|1:'b':c|2:'a':d|"
      row 1 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.fromList [('a',"b"),('b',"c")]
      row 3 (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.empty
      column 'a' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.fromList [(1,"b"),(2,"d")]
      column 'c' (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Map.empty
      rowMap (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
        === Map.fromList [(1, Map.fromList [('a',"b"),('b',"c")]),(2, Map.fromList [('a',"d")])]
      columnMap (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")])
        === Map.fromList [('a', Map.fromList [(1,"b"),(2,"d")]),('b', Map.fromList [(1,"c")])]
      rowKeys (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [1,2]
      columnKeys (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === ['a','b']
      rowKeysSet (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Set.fromList [1,2]
      columnKeysSet (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === Set.fromList ['a','b']
      toList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
      toRowAscList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]
      toColumnAscList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [('a',1,"b"),('a',2,"d"),('b',1,"c")]
      toRowDescList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [(2,'a',"d"),(1,'b',"c"),(1,'a',"b")]
      toColumnDescList (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === [('b',1,"c"),('a',2,"d"),('a',1,"b")]
      Data.Multimap.Table.filter (> "c") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
      Data.Multimap.Table.filter (> "d") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === empty
      filterRow even (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 2 'a' "d"
      filterColumn (> 'a') (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
      filterWithKeys (\r c a -> odd r && c > 'a' && a > "b") (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "c"
      mapMaybe (\a -> if a == "a" then Just "new a" else Nothing) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
        === fromList [(1,'a',"new a"),(2,'b',"new a")]
      let f r c a = if r == 1 && a == "c" then Just "new c" else Nothing in do
        mapMaybeWithKeys f (fromList [(1,'a',"b"),(1,'b',"c"),(2,'a',"d")]) === singleton 1 'b' "new c"
      mapEither (\a -> if a == "a" then Left a else Right a) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
        === (fromList [(1,'a',"a"),(2,'b',"a")],fromList [(1,'b',"c")])
      mapEitherWithKeys (\r c a -> if r == 1 && c == 'a' then Left a else Right a) (fromList [(1,'a',"a"),(1,'b',"c"),(2,'b',"a")])
        === (fromList [(1,'a',"a")],fromList [(1,'b',"c"),(2,'b',"a")])
