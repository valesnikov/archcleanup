module Graphs where

import Data.HashMap.Strict qualified as Map
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

type PackId = Int
type PackIdSet = IntSet
type IdTable a = (a -> PackId, PackId -> a)

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