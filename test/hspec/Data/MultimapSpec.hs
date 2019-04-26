-- Generated code, do not modify by hand. Generate by running "stack build && stack exec test-gen".

{-# OPTIONS_GHC -w #-}
module Data.MultimapSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.Multimap

spec :: Spec
spec = do
  describe "Testing Data.Multimap" $ do
    it "" $ do
      size empty `shouldBe` 0
      singleton 1 'a' `shouldBe` fromList [(1, 'a')]
      size (singleton 1 'a') `shouldBe` 1
      fromList ([] :: [(Int, Char)]) `shouldBe` empty
      fromMap' (Map.fromList [(1, "ab"), (2, ""), (3, "c")]) `shouldBe` fromList [(1, 'a'), (1, 'b'), (3, 'c')]
      fromList [(3, 'a'), (5, 'b'), (3, 'c')] ! 3 `shouldBe` "ac"
