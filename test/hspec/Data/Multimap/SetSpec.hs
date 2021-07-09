-- Generated code, do not modify by hand. Generate by running "stack build && stack exec test-gen".

{-# OPTIONS_GHC -w #-}
module Data.Multimap.SetSpec where

import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Multimap.Set as Data.Multimap.Set
import qualified Data.Multimap as Data.Multimap

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

spec :: Spec
spec = do
  describe "Testing Data.Multimap.Set" $ do
    it "" $ do
      fromMultimap (Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]
