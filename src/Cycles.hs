module Cycles (findCycles) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Array.ST (MArray (getBounds), STArray, newListArray, readArray, writeArray)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as Map
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex)
import Data.Map.Strict qualified
import Data.Maybe (fromJust, isJust)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphs
  ( Graph (..),
    IdTable,
    Pack (pDepends),
    PackId,
    PackIdSet,
    makeIdTable,
  )

data CPack = CPack
  { cpDeps :: Array Int PackId,
    cpFlag :: Bool
  }

type CGraph s = STArray s PackId CPack

toCGraph :: Graph -> ST s (CGraph s, IdTable PackId)
toCGraph (Graph hmap) = do
  arr <- newListArray (Array.bounds table) (map idxToCPack (Array.elems table))
  pure (arr, table)
  where
    (table, rtable) = makeIdTable $ Set.fromList $ Map.keys hmap
    idxToCPack idx = CPack {cpFlag = False, cpDeps = arr $ IntSet.map idxDeps v}
      where
        arr :: IntSet -> Array Int PackId
        arr set = Array.listArray (0, IntSet.size set - 1) (IntSet.toAscList set)
        idxDeps v' = rtable Data.Map.Strict.! v'
        v = pDepends $ hmap Map.! idx

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

markVert :: CGraph s -> PackId -> ST s ()
markVert graph idx = do
  pack <- readArray graph idx
  writeArray graph idx pack {cpFlag = True}

isVertMarked :: CGraph s -> PackId -> ST s Bool
isVertMarked graph idx = readArray graph idx <&> cpFlag

dfs :: CGraph s -> PackId -> [PackId] -> STRef s [[PackId]] -> ST s ()
dfs graph vert path cycles = do
  marked <- isVertMarked graph vert
  if marked
    then when (isJust mbIndex) $ do
      let i = fromJust mbIndex
      modifySTRef cycles (take (i + 1) path :)
    else do
      markVert graph vert
      pack <- readArray graph vert
      forM_
        (cpDeps pack)
        (\v -> dfs graph v (vert : path) cycles)
  where
    mbIndex = elemIndex vert path

findCycles :: Graph -> Set PackIdSet
findCycles g = mergeIntersecting sets
  where
    sets = runST $ do
      (arr, table) <- toCGraph g
      b <- getBounds arr
      resultRef <- newSTRef []
      forM_ [fst b .. snd b] (\v -> dfs arr v [] resultRef)
      result <- readSTRef resultRef
      pure $ Set.fromList $ map (IntSet.fromList . map (table Array.!)) result
