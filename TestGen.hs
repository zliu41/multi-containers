#!/usr/bin/env cabal
{- cabal:
build-depends: base, directory, extra, filepath
-}
-- To run: `./TestGen.hs` or `cabal v2-run TestGen.hs`

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List.Extra (replace, stripPrefix, trim)
import Data.Maybe (mapMaybe)
import System.Directory
import System.FilePath

import Prelude hiding (mod)

data Import = Qualified | Unqualified

type Imports = [(String, String, Import)]

main :: IO ()
main = do
  genTestsFor "Data.Multimap"
    [("Data.Multimap", "Data.Multimap", Unqualified),
     ("Data.Multimap.Set", "Data.Multimap.Set", Qualified)]
  genTestsFor "Data.Multimap.Internal"
    [("Data.Multimap.Internal", "Data.Multimap", Unqualified)]
  genTestsFor "Data.Multimap.Set"
    [("Data.Multimap.Set", "Data.Multimap.Set", Unqualified),
     ("Data.Multimap", "Data.Multimap", Qualified)]
  genTestsFor "Data.Multimap.Set.Internal"
    [("Data.Multimap.Set.Internal", "Data.Multimap.Set", Unqualified)]
  genTestsFor "Data.Multimap.Table.Internal"
    [("Data.Multimap.Table.Internal", "Data.Multimap.Table", Unqualified)]
  genTestsFor "Data.Multimap.Conversion"
    [("Data.Multimap.Conversion", "Data.Multimap.Conversion", Unqualified),
     ("Data.Multimap.Internal", "Data.Multimap", Qualified),
     ("Data.Multimap.Set.Internal", "Data.Multimap.Set", Qualified)]

genTestsFor :: String -> Imports -> IO ()
genTestsFor mod imports = do
  let inputFile = "src" </> replace "." [pathSeparator] mod <.> "hs"
      outputFile = "test/hspec" </> (replace "." [pathSeparator] mod ++ "Spec.hs")
  src <- readFile inputFile
  createDirectoryIfMissing True (takeDirectory outputFile)
  let lns = fmap trim (lines src)
      tests = mapMaybe (stripPrefix "-- > ") lns
  writeFile outputFile . unlines $ header mod imports ++ fmap (indent 6) tests

header :: String -> Imports -> [String]
header mod imports =
  [ "-- Generated code, do not modify by hand. Generate by running \"stack build && stack exec test-gen\"."
  , ""
  , "{-# OPTIONS_GHC -w #-}"
  , "module " ++ mod ++ "Spec where"
  , ""
  , "import Test.Hspec"
  , "import qualified Data.List.NonEmpty as NonEmpty"
  , "import qualified Data.Map as Map"
  , "import qualified Data.Set as Set"
  ] ++ fmap addImport imports ++
  [ ""
  , "(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation"
  , "(===) = shouldBe"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  describe \"Testing " ++ mod ++ "\" $ do"
  , "    it \"\" $ do"
  ]
  where
    addImport (name, as, qual) = "import " ++ qualified qual ++ name ++ " as " ++ as
    qualified = \case Qualified -> "qualified "; Unqualified -> ""

indent :: Int -> String -> String
indent n = (replicate n ' ' ++)
