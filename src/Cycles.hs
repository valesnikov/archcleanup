module Cycles (findCycles) where

import Data.HashMap.Strict qualified as Map
import Data.IntSet qualified as IntSet
import Data.List (elemIndex)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphs (Graph (..), Pack (pDepends, pId), PackId, PackIdSet)

data CPack = CPack
  { cpId :: PackId,
    cpDeps :: PackIdSet,
    cpFlag :: Bool
  }
  deriving (Show)

type CGraph = Map.HashMap PackId CPack

toCGraph :: Graph -> CGraph
toCGraph (Graph hmap) = Map.fromList list
  where
    list = map f $ Map.toList hmap
    f (k, v) =
      (k, CPack {cpId = pId v, cpDeps = pDepends v, cpFlag = False})

markVert :: CPack -> [PackId] -> CGraph -> [[PackId]] -> ([[PackId]], CGraph)
markVert p acc g res = case mbIndex of
  Just i -> (take (i+1) acc : res, g)
  Nothing -> IntSet.foldr dfs (res, markedG) $ cpDeps p
  where
    markedG = Map.adjust (\v -> v {cpFlag = True}) (cpId p) g
    mbIndex = elemIndex (cpId p) acc
    dfs np (rs, gr) = markVert (gr Map.! np) (cpId p:acc) gr rs

mergeIntersecting :: Set PackIdSet -> Set PackIdSet
mergeIntersecting sets
  | Set.null merged = sets
  | otherwise = mergeIntersecting newSets
  where
    (merged, rest) =
      Set.partition
        (\s -> not $ IntSet.null (Set.findMin sets `IntSet.intersection` s))
        (Set.deleteMin sets)
    mergedSet = IntSet.unions merged
    newSets = Set.insert mergedSet rest

findCycles :: Graph -> Set PackIdSet
findCycles g = mergeIntersecting cclsSets
  where
    cmap = toCGraph g
    cclsSets =
      Set.fromList $
        map IntSet.fromList (fst cclsLists)

    cclsLists = foldr finder ([], cmap) cmap
      where
        finder p (res, g') =
          if cpFlag p then (res, g') else markVert p [] g' res
