module Main (main) where

import Data.List.Extra (replace, stripPrefix, trim)
import Data.Maybe (mapMaybe)
import System.Directory
import System.FilePath

import Prelude hiding (mod)

main :: IO ()
main = do
  genTestsFor "Data.Multimap"
  genTestsFor "Data.Multimap.Set"

genTestsFor :: String -> IO ()
genTestsFor mod = do
  let inputFile = "src" </> replace "." [pathSeparator] mod <.> "hs"
      outputFile = "test/hspec" </> (replace "." [pathSeparator] mod ++ "Spec.hs")
  src <- readFile inputFile
  createDirectoryIfMissing True (takeDirectory outputFile)
  let lns = fmap trim (lines src)
      tests = mapMaybe (stripPrefix "-- > ") lns
  writeFile outputFile . unlines $ header mod ++ fmap (indent 6) tests

header :: String -> [String]
header mod =
  [ "-- Generated code, do not modify by hand. Generate by running \"stack build && stack exec test-gen\"."
  , ""
  , "{-# OPTIONS_GHC -w #-}"
  , "module " ++ mod ++ "Spec where"
  , ""
  , "import Test.Hspec"
  , "import qualified Data.List.NonEmpty as NonEmpty"
  , "import qualified Data.Map as Map"
  , "import qualified Data.Set as Set"
  ] ++ ["import " ++ mod] ++
  [ ""
  , "(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation"
  , "(===) = shouldBe"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  describe \"Testing " ++ mod ++ "\" $ do"
  , "    it \"\" $ do"
  ]

indent :: Int -> String -> String
indent n = (replicate n ' ' ++)
