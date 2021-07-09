-- Generated code, do not modify by hand. Generate by running TestGen.hs.

{-# OPTIONS_GHC -w #-}
module Data.MultimapSpec where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Multimap as Data.Multimap
import qualified Data.Multimap.Set as Data.Multimap.Set

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.Multimap" $ do
    it "" $ do
      fromSetMultimapAsc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]
      fromSetMultimapDesc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'b'),(1,'a'),(2,'c')]
