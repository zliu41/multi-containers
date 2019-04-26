{-# OPTIONS_GHC -w #-}

module Main (main) where

import Data.List.Extra (dropEnd, dropWhileEnd, intercalate, isPrefixOf, isSuffixOf, replace, stripPrefix, trim)
import Data.Maybe (mapMaybe)
import System.FilePath

main :: IO ()
main = do
  genTestsFor "Data.Multimap"

genTestsFor :: String -> IO ()
genTestsFor mod = do
  let inputFile = "src" </> replace "." [pathSeparator] mod <.> "hs"
      outputFile = "test/hspec" </> (replace "." [pathSeparator] mod ++ "Spec.hs")
  src <- readFile inputFile
  let lns = fmap trim (lines src)
      tests = fmap (replace "==" "`shouldBe`") . mapMaybe (stripPrefix "-- > ") $ lns
  writeFile outputFile . unlines $ header mod ++ fmap (indent 6) tests

exported :: [String] -> [String]
exported = fmap dropParens
         . words . dropEnd 1 . dropWhileEnd (/= ')') . drop 1 . dropWhile (/= '(')
         . replace "," " " . unlines
         . filter (not . ("--" `isPrefixOf`))
         . dropWhile (not . ("module" `isPrefixOf`))
         . takeUntil ("where" `isSuffixOf`)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

dropParens :: String -> String
dropParens s@('(' : _) | last s == ')' = drop 1 (dropEnd 1 s)
dropParens s = s

header :: String -> [String]
header mod =
  [ "-- Generated code, do not modify by hand. Generate by running \"stack build && stack exec test-gen\"."
  , ""
  , "{-# OPTIONS_GHC -w #-}"
  , "module Data.MultimapSpec where"
  , ""
  , "import Test.Hspec"
  , "import qualified Data.Map as Map"
  ] ++ ["import " ++ mod] ++
  [ ""
  , "spec :: Spec"
  , "spec = do"
  , "  describe \"Testing " ++ mod ++ "\" $ do"
  , "    it \"\" $ do"
  ]

indent :: Int -> String -> String
indent n = (replicate n ' ' ++)
