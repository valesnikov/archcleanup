{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Prepare (verifyDigraph, computeDeps, getCycledVerts, cleanCycles) where

import Data.HashMap.Lazy qualified as Map
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
    unknowns = Map.foldr addFinded Set.empty hmap
    addFinded p acc = acc `Set.union` Set.filter predk packs
      where
        packs = pnrDepends p `Set.union` pnrOptional p
        predk key = not $ Map.member key hmap

-- | Calculates which vertices refer to the given vertex
computeDeps :: Digraph -> Graph
computeDeps (Digraph hmap) = Graph newHmap
  where
    newHmap = fmap newPack hmap
    newPack pack =
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
        newReq = Set.fromList $ Map.keys $ Map.filter reqF hmap
        newOptReq = Set.fromList $ Map.keys $ Map.filter optF hmap
        reqF p = pnrName pack `Set.member` pnrDepends p
        optF p = pnrName pack `Set.member` pnrOptional p

editNeighbors :: Pack -> (Set Text -> Set Text) -> Map.HashMap Text Pack -> Map.HashMap Text Pack
editNeighbors pack f = editOpts . editDeps . editOptReq . editReq
  where
    editDeps hm = foldr edit hm $ pRequired pack
      where
        edit = Map.adjust (\x -> x {pDepends = f $ pDepends x})
    editOpts hm = foldr edit hm $ pOptRequir pack
      where
        edit = Map.adjust (\x -> x {pOptional = f $ pOptional x})
    editReq hm = foldr edit hm $ pDepends pack
      where
        edit = Map.adjust (\x -> x {pRequired = f $ pRequired x})
    editOptReq hm = foldr edit hm $ pOptional pack
      where
        edit = Map.adjust (\x -> x {pOptRequir = f $ pOptRequir x})

-- | Merges multiple vertices into one
mergeVertex :: Set Text -> Graph -> Graph
mergeVertex ps (Graph hmap) = Graph (merge hmap)
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

-- | Returns only vertices that are in a cycle or connect several cycles
getCycledVerts :: Graph -> Graph
getCycledVerts (Graph d) = Graph $ untreeGraph d
  where
    hideVertex pack hmap = Map.delete name $ editNeighbors pack (Set.delete name) hmap
      where
        name = pName pack
    untreeGraph hmap
      | null targets = hmap
      | otherwise = untreeGraph $ foldr hideVertex hmap targets
      where
        targets = Map.filter key hmap
        key pack = (null $ pDepends pack) || (null $ pRequired pack)

-- | Clears the graph of cycles. Vertices participating
-- in a cycle are merged, their size is added up,
-- and dependencies are united
cleanCycles :: Graph -> Forest
cleanCycles = undefined
