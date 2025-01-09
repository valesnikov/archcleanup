module Main where

import Data.Maybe (fromJust)
import Pacman qualified
import Prepare qualified
import Cycles (findCycles)

main :: IO ()
main = do
  (graph, table) <- fromJust <$> Pacman.getGraph
  let af = Prepare.verifyDigraph graph
  case af of
    Left ps -> print ps
    Right dg -> do
      let g1 = Prepare.computeDeps dg
      --let ung = Prepare.untreeGraph g1
  
      --putStr $ Tools.genGraphviz ung table
      print $ findCycles g1
