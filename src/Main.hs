module Main where

import Data.HashMap.Lazy qualified as Map
import Data.Maybe (fromJust)
import Graphs (Graph (Graph))
import Pacman qualified
import Prepare qualified

main :: IO ()
main = do
  graph <- fromJust <$> Pacman.getGraph
  let af = Prepare.verifyDigraph graph
  case af of
    Left ps -> print ps
    Right dg -> do
      let Graph g1 = Prepare.computeDeps dg
      let Graph g2 = Prepare.getCycledVerts (Graph g1)
      print $ Map.keys g2