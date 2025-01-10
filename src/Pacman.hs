module Pacman (available, getGraph) where

import Control.Exception (SomeException, try)
import Data.HashMap.Strict qualified as Map
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Graphs (IdTable)
import Graphs qualified
import Tools qualified

-- | Structure of the pacman package
data Pack = Pack
  { pName :: Text,
    pSize :: Integer,
    pExplicit :: Bool,
    pProvides :: Set Text,
    pDepends :: Set Text,
    pOptional :: Set Text
  }
  deriving (Show)

-- | Graph for internal computing
type PacDigraph = Map.HashMap Text Pack

-- | Name of the data extraction program
expacName :: Text
expacName = "expac"

-- | Checks the system for pacman and expac
available :: IO Bool
available = do
  pac <- Tools.checkExecutable "pacman"
  expac <- Tools.checkExecutable expacName
  return $ pac && expac

-- | Сall 'expac' with the given parameters
getExpacQuery :: [Text] -> IO (Maybe Text)
getExpacQuery args = do
  exRes <- try $ Tools.getCmdOutput expacName ("-Q" : args)
  case exRes of
    Left ex -> do
      print (ex :: SomeException)
      pure Nothing
    Right r -> pure r

-- | Parses lines and blocks into packets
parseToPackages :: [[Text]] -> Maybe [Pack]
parseToPackages = mapM parse
  where
    parse [name, depends, provides, optional, explicit, size] =
      Just $
        Pack
          { pName = name,
            pSize = read (Text.unpack size),
            pExplicit = explicit == "explicit",
            pProvides = Set.fromList $ Text.words provides,
            pDepends = Set.fromList $ Text.words depends,
            pOptional = Set.fromList $ Text.words optional
          }
    parse _ = Nothing

-- | creates a graph from the list of packages
createGraph :: [Pack] -> PacDigraph
createGraph = Map.fromList . map (\x -> (pName x, x))

-- | Creates a generic graph from the pacman graph (discards provides)
convertGraph :: PacDigraph -> (Graphs.UnverifiedDigraph, IdTable Text)
convertGraph d = (Graphs.UnverifiedDigraph $ Map.map convPack idKeyMap, table)
  where
    (table, rtable) = Graphs.makeIdTable $ Set.fromList $ Map.keys d
    idKeyMap = Map.mapKeys (rtable Data.Map.Strict.!) d
    convPack :: Pack -> Graphs.PackNoReq
    convPack p =
      Graphs.PackNoReq
        { Graphs.pnrId = rtable Data.Map.Strict.! pName p,
          Graphs.pnrSize = pSize p,
          Graphs.pnrExplicit = pExplicit p,
          Graphs.pnrDepends = IntSet.fromList $ map (rtable Data.Map.Strict.!) $ Set.elems $ pDepends p,
          Graphs.pnrOptional = IntSet.fromList $ map (rtable Data.Map.Strict.!) $ Set.elems $ pOptional p
        }

-- | Map the 'provides' elements to the package name
type ProvidesMap = Map.HashMap Text Text

-- | Makes a map of provide and name
provideToName :: PacDigraph -> ProvidesMap
provideToName = foldr addPack Map.empty
  where
    addPack pack mp =
      Map.insert (pName pack) (pName pack) $
        foldr addProv mp (pProvides pack)
      where
        addProv prov = Map.insert prov (pName pack)

-- | Removes uninstalled optional dependencies
-- and replaces 'provides' with the matching 'name'
cleanOpts :: PacDigraph -> ProvidesMap -> PacDigraph
cleanOpts d pmap = Map.map edit d
  where
    edit v = v {pOptional = provToName (pOptional v)}
    provToName = Set.map (pmap Map.!) . delUninst
    delUninst = Set.filter (`Map.member` pmap)

-- | Replaces 'provides' with the matching 'name'
cleanDeps :: PacDigraph -> ProvidesMap -> PacDigraph
cleanDeps d pmap = Map.map edit d
  where
    edit v = v {pDepends = provToName (pDepends v)}
    provToName = Set.map (\x -> verify x $ pmap Map.!? x)
      where
        verify name Nothing =
          error $
            "Dependency "
              ++ show name
              ++ " not found,\
                 \ apparently the package base is broken"
        verify _ (Just p) = p

-- | Gets the graph through pacman and expac
getGraph :: IO (Maybe (Graphs.UnverifiedDigraph, IdTable Text))
getGraph = do
  -- name, depends on, provides, optional deps, install reason, install size
  mbPack <- getExpacQuery ["%n|%E|%S|%o|%w|%m"]
  case (parseToPackages . map (Text.splitOn "|") . Text.lines) =<< mbPack of
    Nothing -> pure Nothing
    Just packs -> do
      let graph = createGraph packs
      let provMap = provideToName graph
      let cleanedOpt = cleanOpts graph provMap
      let cleanedDeps = cleanDeps cleanedOpt provMap

      pure $ Just (convertGraph cleanedDeps)