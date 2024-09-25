module Main where

import Algs qualified
import Data.Maybe (fromJust)
import Pacman qualified

main :: IO ()
main = do
  graph <- fromJust <$> Pacman.getGraph
  let a = Algs.verifyDigraph graph
  print a