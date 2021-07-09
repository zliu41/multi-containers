-- Generated code, do not modify by hand. Generate by running "stack build && stack exec test-gen".

{-# OPTIONS_GHC -w #-}
module Data.Multimap.ConversionsSpec where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Multimap.Conversions as Data.Multimap.Conversions
import qualified Data.Multimap.Internal as Data.Multimap
import qualified Data.Multimap.Set.Internal as Data.Multimap.Set

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.Multimap.Conversions" $ do
    it "" $ do
      toMultimapAsc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]
      toMultimapDesc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'b'),(1,'a'),(2,'c')]
      toSetMultimap (Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]
