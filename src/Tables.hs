module Tables where

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

-- | Packages with computed dependencies and deleted loops
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

type BaseNoReq = Map.Map Text PackNoReq

type Base = Map.Map Text Pack

-- | Digraph has not yet been tested for unknown dependencies
newtype UnverifiedDigraph = UnverifiedDigraph BaseNoReq deriving Show

-- | Package dependency graph, with cyclic dependencies
newtype Digraph = Digraph BaseNoReq deriving Show

-- | A digraph in which cyclic dependencies are united in one vertex
newtype Forest = Forest BaseNoReq deriving Show

-- | A forest in which for each vertex it indicates who depends on it
newtype BidirForest = BidirForest Base deriving Show

-- UnverifiedDigraph -> Digraph -> Forest -> BidirForest