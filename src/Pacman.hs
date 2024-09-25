{-# LANGUAGE OverloadedStrings #-}

module Pacman (getGraph) where

import Control.Exception (SomeException, try)
import Data.HashMap qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
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
  deriving (Eq, Show)

-- | Graph for internal computing
type PacDigraph = Map.Map Text Pack

-- | Name of the data extraction program
expacName :: Text
expacName = "expac"

-- | Сall 'expac' with the given parameters
getExpacQuery :: [Text] -> IO (Maybe Text)
getExpacQuery args = do
  exRes <- try $ Tools.getCmdOutput expacName ("-Q" : args)
  case exRes of
    Left (ex :: SomeException) -> do
      print ex
      pure Nothing
    Right r -> pure r

-- | Parses lines and blocks into packets
parseToPackages :: [[Text]] -> Maybe [Pack]
parseToPackages = mapM parse
  where
    parse [name', depends', provides', optional', explicit', size'] =
      Just $
        Pack
          { pName = name',
            pSize = read (Text.unpack size'),
            pExplicit = explicit' == "explicit",
            pProvides = Set.fromList $ Text.words provides',
            pDepends = Set.fromList $ Text.words depends',
            pOptional = Set.fromList $ Text.words optional'
          }
    parse _ = Nothing

-- | creates a graph from the list of packages
createGraph :: [Pack] -> PacDigraph
createGraph = Map.fromList . map (\x -> (pName x, x))

-- | Creates a generic graph from the pacman graph (discards provides)
converGraph :: PacDigraph -> Graphs.UnverifiedDigraph
converGraph d = Graphs.UnverifiedDigraph $ Map.map convPack d
  where
    convPack :: Pack -> Graphs.PackNoReq
    convPack p =
      Graphs.PackNoReq
        { Graphs.pnrName = pName p,
          Graphs.pnrSize = pSize p,
          Graphs.pnrExplicit = pExplicit p,
          Graphs.pnrDepends = pDepends p,
          Graphs.pnrOptional = pOptional p
        }

-- | Map the 'provides' elements to the package name
type ProvidesMap = Map.Map Text Text

-- | Makes a map of provide and name
provideToName :: PacDigraph -> ProvidesMap
provideToName = Map.fold addPack Map.empty
  where
    addPack pack mp =
      Map.insert (pName pack) (pName pack) $
        Set.fold addProv mp (pProvides pack)
      where
        addProv prov = Map.insert prov (pName pack)

-- | Removes uninstalled optional dependencies
-- and replaces 'provides' with the matching 'name'
cleanOpts :: PacDigraph -> ProvidesMap -> PacDigraph
cleanOpts d pmap = Map.map edit d
  where
    edit v = v {pOptional = provToName (pOptional v)}
    provToName = Set.map (fromJust . (`Map.lookup` pmap)) . delUninst
    delUninst = Set.filter (`Map.member` pmap)

-- | Replaces 'provides' with the matching 'name'
cleanDeps :: PacDigraph -> ProvidesMap -> PacDigraph
cleanDeps d pmap = Map.map edit d
  where
    edit v = v {pDepends = provToName (pDepends v)}
    provToName = Set.map (\x -> verify x $ Map.lookup x pmap)
      where
        verify :: Text -> Maybe Text -> Text
        verify name Nothing =
          error $
            "Dependency "
              ++ show name
              ++ " not found,\
                 \ apparently the package base is broken"
        verify _ (Just p) = p

-- | Gets the graph through pacman.
-- The only function in the module visible to other packages
getGraph :: IO (Maybe Graphs.UnverifiedDigraph)
getGraph = do
  mbPack <- getExpacQuery ["%n|%E|%S|%o|%w|%m"]
  case (parseToPackages . map (Text.splitOn "|") . Text.lines) =<< mbPack of
    Nothing -> pure Nothing
    Just packs -> do
      let graph = createGraph packs
      let provMap = provideToName graph
      let cleanedOpt = cleanOpts graph provMap
      let cleanedDeps = cleanDeps cleanedOpt provMap
      pure $ Just $ converGraph cleanedDeps
