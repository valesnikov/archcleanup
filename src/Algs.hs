{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Algs (verifyDigraph, computeDeps) where

import Data.HashMap qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Graphs

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

-- | Calculates which vertices refer to the given vertex
computeDeps :: Digraph -> Graph
computeDeps (Digraph hmap) = Graph newHmap
  where
    newHmap = Map.map f hmap
    f pack =
      Pack
        { pName = pnrName pack,
          pSize = pnrSize pack,
          pExplicit = pnrExplicit pack,
          pDepends = pnrDepends pack,
          pOptional = pnrOptional pack,
          pRequired = newReq,
          pOptRequir = newOptReq
        }
      where
        newReq = Map.keysSet $ Map.filter reqF hmap
        newOptReq = Map.keysSet $ Map.filter optF hmap
        reqF p = Set.member (pnrName pack) (pnrDepends p)
        optF p = Set.member (pnrName pack) (pnrOptional p)

mergeVertex :: Set Text -> Graph -> Graph
mergeVertex st (Graph hmap) = Graph (merge hmap)
  where
    merge =
      Map.insert newName newVert
        . editOpts
        . editDeps
        . editOptReq
        . editReq
        . delPacks

    delPacks = Map.filter predk
      where
        predk p = Set.notMember (pName p) st

    newName = Text.intercalate "|" $ Set.toList st
    extract field = field . fromJust . (`Map.lookup` hmap)

    mergeFields f = Set.difference (Set.unions $ Set.map (extract f) st) st
    replaceNames f pack = Set.insert newName $ Set.difference (f pack) st

    editDeps hmap' = Set.fold edit hmap' $ mergeFields pRequired
      where
        edit = Map.adjust (\x -> x {pDepends = replaceNames pDepends x})

    editOpts hmap' = Set.fold edit hmap' $ mergeFields pOptRequir
      where
        edit = Map.adjust (\x -> x {pOptional = replaceNames pOptional x})

    editReq hmap' = Set.fold edit hmap' $ mergeFields pDepends
      where
        edit = Map.adjust (\x -> x {pRequired = replaceNames pRequired x})

    editOptReq hmap' = Set.fold edit hmap' $ mergeFields pOptional
      where
        edit = Map.adjust (\x -> x {pOptRequir = replaceNames pOptRequir x})

    newVert =
      Pack
        { pName = newName,
          pSize = sum $ Set.map (extract pSize) st,
          pExplicit = Set.fold ((||) . extract pExplicit) False st,
          pDepends = mergeFields pDepends,
          pOptional = mergeFields pOptional,
          pRequired = mergeFields pRequired,
          pOptRequir = mergeFields pOptRequir
        }

-- | Clears the graph of cycles. Vertices participating
-- in a cycle are merged, their size is added up,
-- and dependencies are united
cleanCycles :: Graph -> Forest
cleanCycles (Graph d) = Forest $ undefined d