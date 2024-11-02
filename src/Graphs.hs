module Graphs where

import Data.HashMap.Lazy qualified as Map
import Data.Set (Set)
import Data.Text (Text)

-- | Packages without computed dependencies
-- each entry in 'optional' and 'depends' must match an existing 'name'
data PackNoReq = PackNoReq
  { pnrName :: Text,
    pnrSize :: Integer,
    pnrExplicit :: Bool,
    pnrDepends :: Set Text,
    pnrOptional :: Set Text
  }
  deriving (Show)

-- | Packages with computed dependencies and opt dependencies
data Pack = Pack
  { pName :: Text,
    pSize :: Integer,
    pExplicit :: Bool,
    pDepends :: Set Text,
    pOptional :: Set Text,
    pRequired :: Set Text,
    pOptRequir :: Set Text
  }
  deriving (Show)

-- | Digraph has not yet been tested for unknown dependencies
newtype UnverifiedDigraph
  = UnverifiedDigraph (Map.HashMap Text PackNoReq)
  deriving (Show)

-- | Package dependency graph, with cyclic dependencies
newtype Digraph
  = Digraph (Map.HashMap Text PackNoReq)
  deriving (Show)

-- | The vertices connected to the given vertex are computed
newtype Graph
  = Graph (Map.HashMap Text Pack)
  deriving (Show)

-- | A digraph in which cyclic dependencies are merged in one vertex
newtype Forest
  = Forest (Map.HashMap Text Pack)
  deriving (Show)

-- UnverifiedDigraph -> Digraph -> Graph -> Forest