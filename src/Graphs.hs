module Graphs where

import Data.HashMap qualified as Map
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Digraph has not yet been tested for unknown dependencies
newtype UnverifiedDigraph
  = UnverifiedDigraph (Map.Map Text PackNoReq)
  deriving (Show)

-- | Package dependency graph, with cyclic dependencies
newtype Digraph
  = Digraph (Map.Map Text PackNoReq)
  deriving (Show)

-- | The vertices connected to the given vertex are computed
newtype Graph
  = Graph (Map.Map Text Pack)
  deriving (Show)

-- | A digraph in which cyclic dependencies are merged in one vertex
newtype Forest
  = Forest (Map.Map Text Pack)
  deriving (Show)

-- UnverifiedDigraph -> Digraph -> Graph -> Forest