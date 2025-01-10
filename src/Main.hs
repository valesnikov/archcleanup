module Main where

import Control.Monad (forM_)
import Cycles (findCycles)
import Data.Array qualified as Array
import Data.IntSet qualified as IntSet
import Data.Maybe (fromJust)
import Pacman qualified
import Prepare qualified

main :: IO ()
main = do
  (graph, table) <- fromJust <$> Pacman.getGraph
  let af = Prepare.verifyDigraph graph
  case af of
    Left ps -> print ps
    Right dg -> do
      let g1 = Prepare.computeDeps dg
      -- let ung = Prepare.untreeGraph g1

      let ccls = findCycles g1
      forM_ ccls $ print . map (table Array.!) . IntSet.toAscList
