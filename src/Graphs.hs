module Graphs where

import Data.Array (Array)
import Data.Array qualified as Array
import Data.HashMap.Strict qualified as Map
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified
import Data.Set (Set)
import Data.Set qualified as Set

type PackId = Int

type PackIdSet = IntSet

type IdTable a = Array Int a

type ReverseIdTable a = Data.Map.Strict.Map a Int

makeIdTable :: (Ord a) => Set a -> (IdTable a, ReverseIdTable a)
makeIdTable set = (fromId, toId)
  where
    list = Set.toAscList set
    toId = Data.Map.Strict.fromList $ zip list [0 ..]
    fromId = Array.listArray (0, length list - 1) list

-- | Packages without computed dependencies
-- each entry in 'optional' and 'depends' must match an existing 'name'
data PackNoReq = PackNoReq
  { pnrId :: PackId,
    pnrSize :: Integer,
    pnrExplicit :: Bool,
    pnrDepends :: PackIdSet,
    pnrOptional :: PackIdSet
  }
  deriving (Show)

-- | Packages with computed dependencies and opt dependencies
data Pack = Pack
  { pId :: PackId,
    pSize :: Integer,
    pExplicit :: Bool,
    pDepends :: PackIdSet,
    pOptional :: PackIdSet,
    pRequired :: PackIdSet,
    pOptRequir :: PackIdSet
  }
  deriving (Show, Eq, Ord)

-- | Neighbors of vertex
pNeighbors :: Pack -> PackIdSet
pNeighbors p = IntSet.union (pDepends p) (pRequired p)

-- | Optional neighbors of vertex
pOptNeighb :: Pack -> PackIdSet
pOptNeighb p = IntSet.union (pOptional p) (pOptRequir p)

-- | Digraph has not yet been tested for unknown dependencies
newtype UnverifiedDigraph
  = UnverifiedDigraph (Map.HashMap PackId PackNoReq)
  deriving (Show)

-- | Package dependency graph, with cyclic dependencies
newtype Digraph
  = Digraph (Map.HashMap PackId PackNoReq)
  deriving (Show)

-- | The vertices connected to the given vertex are computed
newtype Graph
  = Graph (Map.HashMap PackId Pack)
  deriving (Show, Eq, Ord)

-- | A digraph in which cyclic dependencies are merged in one vertex
newtype Forest
  = Forest (Map.HashMap PackId Pack)
  deriving (Show)

-- UnverifiedDigraph -> Digraph -> Graph -> Forest