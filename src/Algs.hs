module Algs where

import Data.HashMap qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Tables

-- | Checks the graph for unknown packets and returns them
verifyDigraph :: UnverifiedDigraph -> Either (Set Text) Digraph
verifyDigraph (UnverifiedDigraph hmap)
  | Set.null unknowns = Right $ Digraph hmap
  | otherwise = Left unknowns
  where
    unknowns = Map.fold addFinded Set.empty hmap
    addFinded p acc = acc `Set.union` Set.filter predk packs
      where
        packs = pnrDepends p `Set.union` pnrOptional p
    predk key = Map.notMember key hmap