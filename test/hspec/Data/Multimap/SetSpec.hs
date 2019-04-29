-- Generated code, do not modify by hand. Generate by running "stack build && stack exec test-gen".

{-# OPTIONS_GHC -w #-}
module Data.Multimap.SetSpec where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Multimap.Set

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.Multimap.Set" $ do
    it "" $ do
      size empty === 0
      singleton 1 'a' === fromList [(1, 'a')]
      size (singleton 1 'a') === 1
      fromList ([] :: [(Int, Char)]) === empty
      fromList [(1, 'b'), (2, 'a'), (1, 'b')] === fromList [(1, 'b'), (2, 'a')]
      insert 1 'a' empty === singleton 1 'a'
      insert 1 'a' (fromList [(1, 'b'), (2, 'a')]) === fromList [(1, 'a'), (1, 'b'), (2, 'a')]
      insert 1 'a' (fromList [(1, 'a'), (2, 'c')]) === fromList [(1, 'a'), (2, 'c')]
      delete 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === singleton 2 'c'
      deleteWithValue 1 'c' (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
      deleteWithValue 1 'c' (fromList [(2,'c'),(1,'c')]) === singleton 2 'c'
      deleteMax 3 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
      deleteMax 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(2,'c')]
      deleteMin 3 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'a'),(1,'b'),(2,'c')]
      deleteMin 1 (fromList [(1,'a'),(1,'b'),(2,'c')]) === fromList [(1,'b'),(2,'c')]
      adjust ("new " ++) 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"new a"),(1,"new b"),(2,"c")]
      adjust (const "z") 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"z"),(2,"c")]
      adjustWithKey (\k x -> show k ++ ":new " ++ x) 1 (fromList [(1,"a"),(1,"b"),(2,"c")])
        === fromList [(1,"1:new a"),(1,"1:new b"),(2,"c")]
      let f x = if x == "a" then Just "new a" else Nothing in do
        update f 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"new a"),(2, "c")]
        update f 1 (fromList [(1,"b"),(1,"c"),(2,"c")]) === singleton 2 "c"
      let f k x = if x == "a" then Just (show k ++ ":new a") else Nothing in do
        updateWithKey f 1 (fromList [(1,"a"),(1,"b"),(2,"c")]) === fromList [(1,"1:new a"),(2,"c")]
        updateWithKey f 1 (fromList [(1,"b"),(1,"c"),(2,"c")]) === singleton 2 "c"
      let (f, g) = (const Set.empty, Set.insert 'c') in do
        alter f 1 (fromList [(1, 'a'), (2, 'b')]) === singleton 2 'b'
        alter f 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b')]
        alter g 1 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'c'), (1, 'a'), (2, 'b')]
        alter g 1 (fromList [(1, 'c'), (2, 'b')]) === fromList [(1, 'c'), (2, 'b')]
        alter g 3 (fromList [(1, 'a'), (2, 'b')]) === fromList [(1, 'a'), (2, 'b'), (3, 'c')]
      let (f, g) = (const (const Set.empty), Set.insert . show) in do
        alterWithKey f 1 (fromList [(1, "a"), (2, "b")]) === singleton 2 "b"
        alterWithKey f 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b")]
        alterWithKey g 1 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "1"), (1, "a"), (2, "b")]
        alterWithKey g 3 (fromList [(1, "a"), (2, "b")]) === fromList [(1, "a"), (2, "b"), (3, "3")]
      fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 3 === Set.fromList "ac"
      fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 2 === Set.empty
      member 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === True
      member 1 (deleteMax 1 (fromList [(2, 'c'), (1, 'c')])) === False
      notMember 1 (fromList [(1, 'a'), (2, 'b'), (2, 'c')]) === False
      notMember 1 (deleteMin 1 (fromList [(2, 'c'), (1, 'c')])) === True
      Data.Multimap.Set.null empty === True
      Data.Multimap.Set.null (singleton 1 'a') === False
      notNull empty === False
      notNull (singleton 1 'a') === True
      size empty === 0
      size (singleton 1 'a') === 1
      size (fromList [(1, 'a'), (2, 'b'), (2, 'c'), (2, 'b')]) === 3
      union (fromList [(1,'a'),(2,'b'),(2,'c')]) (fromList [(1,'d'),(2,'b')])
        === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c')]
      unions [fromList [(1,'a'),(2,'b'),(2,'c')], fromList [(1,'d'),(2,'b')]]
        === fromList [(1,'a'),(1,'d'),(2,'b'),(2,'c')]
      difference (fromList [(1,'a'),(2,'b'),(2,'c')]) (fromList [(1,'d'),(2,'b'),(2,'a')])
        === fromList [(1,'a'),(2,'c')]
      Data.Multimap.Set.map (++ "x") (fromList [(1,"a"),(2,"b")]) === fromList [(1,"ax"),(2,"bx")]
      Data.Multimap.Set.map (const "c") (fromList [(1,"a"),(1,"b"),(2,"b")]) === fromList [(1,"c"),(2,"c")]
      mapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1,"a"),(2,"b")]) === fromList [(1,"1:a"),(2,"2:b")]
      Data.Multimap.Set.foldr ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
      Data.Multimap.Set.foldl (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
      foldrWithKey (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
      foldlWithKey (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
      Data.Multimap.Set.foldr' ((+) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
      Data.Multimap.Set.foldl' (\len -> (+ len) . length) 0 (fromList [(1, "hello"), (1, "world"), (2, "!")]) === 11
      foldrWithKey' (\k a len -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
      foldlWithKey' (\len k a -> length (show k) + length a + len) 0 (fromList [(1, "hello"), (1, "world"), (20, "!")]) === 15
      foldMapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1, "a"), (1, "c"), (2, "b")]) === "1:a1:c2:b"
      elems (fromList [(2,'a'),(1,'b'),(3,'d'),(3,'c'),(1,'b')]) === "bacd"
      elems (empty :: SetMultimap Int Char) === []
      keys (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'b')]) === [1,2,3]
      keys (empty :: SetMultimap Int Char) === []
      keysSet (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'b')]) === Set.fromList [1,2,3]
      keysSet (empty :: SetMultimap Int Char) === Set.empty
      toList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'a'),(1,'b'),(2,'a'),(3,'c')]
      toAscList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(1,'a'),(1,'b'),(2,'a'),(3,'c')]
      toDescList (fromList [(2,'a'),(1,'b'),(3,'c'),(1,'a')]) === [(3,'c'),(2,'a'),(1,'b'),(1,'a')]
      Data.Multimap.Set.filter (> 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === singleton 1 'b'
      Data.Multimap.Set.filter (< 'a') (fromList [(1,'a'),(1,'b'),(2,'a')]) === empty
      filterWithKey (\k a -> even k && a > 'a') (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'b')]) === singleton 2 'b'
      let f a | a > 'b' = Just True
              | a < 'b' = Just False
              | a == 'b' = Nothing
       in do
         filterM f (fromList [(1,'a'),(1,'b'),(2,'a'),(2,'c')]) === Nothing
         filterM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Just (fromList [(1,'c'),(2,'c')])
      let f k a | even k && a > 'b' = Just True
                | odd k && a < 'b' = Just False
                | otherwise = Nothing
       in do
         filterWithKeyM f (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')]) === Nothing
         filterWithKeyM f (fromList [(1,'a'),(3,'a'),(2,'c'),(4,'c')]) === Just (fromList [(2,'c'),(4,'c')])
      mapMaybe (\a -> if a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
        === fromList [(1,"new a"),(2,"new a")]
      mapMaybeWithKey (\k a -> if k > 1 && a == "a" then Just "new a" else Nothing) (fromList [(1,"a"),(1,"b"),(2,"a"),(2,"c")])
        === singleton 2 "new a"
      mapEither (\a -> if a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
        === (fromList [(1,'a'),(2,'a')],fromList [(1,'c'),(2,'c')])
      mapEitherWithKey (\k a -> if even k && a < 'b' then Left a else Right a) (fromList [(1,'a'),(1,'c'),(2,'a'),(2,'c')])
        === (fromList [(2,'a')],fromList [(1,'a'),(1,'c'),(2,'c')])
