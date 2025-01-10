{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Prepare where

import Data.HashMap.Strict qualified as Map
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (uncons)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphs

-- | Checks the graph for unknown packets and returns them
verifyDigraph :: UnverifiedDigraph -> Either IntSet Digraph
verifyDigraph (UnverifiedDigraph hmap)
  | IntSet.null unknowns = Right $ Digraph hmap
  | otherwise = Left unknowns
  where
    unknowns = Map.foldr addFinded IntSet.empty hmap
    addFinded p acc = acc `IntSet.union` IntSet.filter predk packs
      where
        packs = pnrDepends p `IntSet.union` pnrOptional p
        predk key = not $ Map.member key hmap

-- | Calculates which vertices refer to the given vertex
computeDeps :: Digraph -> Graph
computeDeps (Digraph hmap) = Graph newHmap
  where
    newHmap = fmap newPack hmap
    newPack pack =
      Pack
        { pId = pnrId pack,
          pSize = pnrSize pack,
          pExplicit = pnrExplicit pack,
          pDepends = pnrDepends pack,
          pOptional = pnrOptional pack,
          pRequired = newReq,
          pOptRequir = newOptReq
        }
      where
        newReq = IntSet.fromList $ Map.keys $ Map.filter reqF hmap
        newOptReq = IntSet.fromList $ Map.keys $ Map.filter optF hmap
        reqF p = pnrId pack `IntSet.member` pnrDepends p
        optF p = pnrId pack `IntSet.member` pnrOptional p

editNeighbors :: Pack -> (IntSet -> IntSet) -> Map.HashMap PackId Pack -> Map.HashMap PackId Pack
editNeighbors pack f = editOpts . editDeps . editOptReq . editReq
  where
    editDeps hm = IntSet.foldr edit hm $ pRequired pack
      where
        edit = Map.adjust (\x -> x {pDepends = f $ pDepends x})
    editOpts hm = IntSet.foldr edit hm $ pOptRequir pack
      where
        edit = Map.adjust (\x -> x {pOptional = f $ pOptional x})
    editReq hm = IntSet.foldr edit hm $ pDepends pack
      where
        edit = Map.adjust (\x -> x {pRequired = f $ pRequired x})
    editOptReq hm = IntSet.foldr edit hm $ pOptional pack
      where
        edit = Map.adjust (\x -> x {pOptRequir = f $ pOptRequir x})

-- | Merges multiple vertices into one
-- Package names are joined into one,
-- size is summed, and all dependencies are unioned

{-
mergeVertices :: Set Text -> Graph -> Graph
mergeVertices ps (Graph hmap) = Graph (merge hmap)
  where
    merge =
      Map.insert newName newVert
        . (editNeighbors newVert replaceNames)
        . delPacks
    delPacks = Map.filter (not . (`Set.member` ps) . pName)
    newName = Text.intercalate "|" $ Set.elems ps
    extract field = field . (hmap Map.!)
    mergeFields f = Set.difference (Set.unions $ Set.map (extract f) ps) ps
    replaceNames packs = Set.insert newName $ Set.difference packs ps
    newVert =
      Pack
        { pName = newName,
          pSize = sum $ Set.map (extract pSize) ps,
          pExplicit = foldr ((||) . extract pExplicit) False ps,
          pDepends = mergeFields pDepends,
          pOptional = mergeFields pOptional,
          pRequired = mergeFields pRequired,
          pOptRequir = mergeFields pOptRequir
        }
-}

-- | Returns only vertices that are in a cycle or connect several cycles
-- It removes packages that have no dependencies or are not someone's
-- dependencies, and repeats as long as there are such packages.
-- Also includes nodes that connect loops but are not included in them
untreeGraph :: Graph -> Graph
untreeGraph (Graph d) = Graph $ untree d
  where
    hideVertex pack = Map.delete name . editNeighbors pack (IntSet.delete name)
      where
        name = pId pack
    untree hmap
      | null targets = hmap
      | otherwise = untree $ foldr hideVertex hmap targets
      where
        targets = Map.filter prdc hmap
        prdc pack = IntSet.null (pDepends pack) || IntSet.null (pRequired pack)

getComponents :: Graph -> Set Graph
getComponents (Graph hmap) = Set.map Graph $ splitComps hmap Set.empty
  where
    splitComps hm acc = case unconsComp hm of
      Nothing -> acc
      Just (c, cs) -> splitComps cs (Set.insert c acc)

    unconsComp hm = case getSomeElem hm of
      Nothing -> Nothing
      Just idx ->
        let comp = component idx Map.empty hm
         in Just (comp, Map.difference hm comp)

    component idx acc hm
      | Map.member idx acc = acc
      | otherwise = IntSet.foldr (\n st -> component n st hm) newMap neibh
      where
        neibh = pNeighbors $ hm Map.! idx
        newMap = Map.insert idx (hm Map.! idx) acc

    getSomeElem = fmap fst . uncons . Map.keys
