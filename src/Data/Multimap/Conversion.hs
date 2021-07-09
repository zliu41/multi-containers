-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Multimap.Conversion
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Conversion between 'Multimap' and 'SetMultimap'.
module Data.Multimap.Conversion (
  toMultimapAsc
  , toMultimapDesc
  , toSetMultimap
) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Multimap.Internal (Multimap (..))
import Data.Multimap.Set.Internal (SetMultimap (..))
import qualified Data.Set as Set

-- | Convert a t'Data.Multimap.Set.SetMultimap' to a t'Data.Multimap.Multimap' where the values of each key
-- are in ascending order.
--
-- > toMultimapAsc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]
toMultimapAsc :: SetMultimap k a -> Multimap k a
toMultimapAsc (SetMultimap (m, sz)) = Multimap (m', sz)
  where
    m' = Map.mapMaybe (NonEmpty.nonEmpty . Set.toAscList) m

-- | Convert a t'Data.Multimap.Set.SetMultimap' to a t'Data.Multimap.Multimap' where the values of each key
-- are in descending order.
--
-- > toMultimapDesc (Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.fromList [(1,'b'),(1,'a'),(2,'c')]
toMultimapDesc :: SetMultimap k a -> Multimap k a
toMultimapDesc (SetMultimap (m, sz)) = Multimap (m', sz)
  where
    m' = Map.mapMaybe (NonEmpty.nonEmpty . Set.toDescList) m

-- | Convert a t'Data.Multimap.Multimap' to a t'Data.Multimap.Set.SetMultimap'.
--
-- > toSetMultimap (Data.Multimap.fromList [(1,'a'),(1,'b'),(2,'c')]) === Data.Multimap.Set.fromList [(1,'a'),(1,'b'),(2,'c')]
toSetMultimap :: Ord a => Multimap k a -> SetMultimap k a
toSetMultimap (Multimap (m, sz)) = SetMultimap (m', sz)
  where
    m' = Map.map (Set.fromList . NonEmpty.toList) m
